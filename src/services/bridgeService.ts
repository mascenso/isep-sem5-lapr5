import { Service, Inject } from 'typedi';
import config from "../../config";
import IBridgeDTO from '../dto/IBridgeDTO';
import { Bridge } from "../domain/bridge";
import IBridgeRepo from '../services/IRepos/IBridgeRepo';
import IBridgeService from './IServices/IBridgeService';
import { Result } from "../core/logic/Result";
import { BridgeMap } from "../mappers/BridgeMap";
import IFloorRepo from "./IRepos/IFloorRepo";
import IBuildingBridgeDTO from '../dto/IBuildingBridgeDTO';
import IBuildingRepo from './IRepos/IBuildingRepo';
import { BuildingBridge } from '../domain/buildingBridge';

@Service()
export default class BridgeService implements IBridgeService {
  constructor(
    @Inject(config.repos.bridge.name) private bridgeRepo: IBridgeRepo,
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
  ) { }

  public async getBridge(bridgeId: string): Promise<Result<IBridgeDTO>> {
    try {
      const bridge = await this.bridgeRepo.findByDomainId(bridgeId);

      if (bridge === null) {
        return Result.fail<IBridgeDTO>("Bridge not found");
      }
      else {
        const bridgeDTOResult = BridgeMap.toDTO(bridge) as IBridgeDTO;
        return Result.ok<IBridgeDTO>(bridgeDTOResult)
      }
    } catch (e) {
      throw e;
    }
  }


  public async createBridge(bridgeDTO: IBridgeDTO): Promise<Result<IBridgeDTO>> {
    try {

      /* check if floors exist */
      const floorAId = await this.floorRepo.findByDomainId(bridgeDTO.floorAId);
      const floorBId = await this.floorRepo.findByDomainId(bridgeDTO.floorBId);
      if (floorAId === null || floorBId === null) {
        return Result.fail<IBridgeDTO>('Floor not found');
      }

      /* retrieve building ids */
      const buildingAId = floorAId.buildingId;
      const buildingBId = floorBId.buildingId;

      /* create bridge */
      const bridgeOrError = await Bridge.create(bridgeDTO);

      if (bridgeOrError.isFailure) {
        return Result.fail<IBridgeDTO>(bridgeOrError.errorValue());
      }

      /* before saving, check if bridge already exists */
      if (await this.bridgeRepo.areConnected(bridgeDTO.floorAId, bridgeDTO.floorBId))
        // Combinação já existente
      {
        return Result.fail<IBridgeDTO>("Bridge already exists");
      }
      else if (buildingAId === buildingBId)
       // Nao podem estar no mesmo building
      {
        return Result.fail<IBridgeDTO>('Bridge cannot connect floors of the same building');
      }
      else {
        // Criação e persistência do novo objeto Bridge
        const bridgeResult = bridgeOrError.getValue();
        bridgeResult.buildingBId = buildingBId;
        bridgeResult.buildingAId = buildingAId;

        /* Salva a bridge com os ids dos buildings correspondentes */
        await this.bridgeRepo.save(bridgeResult);

        const bridgeDTOResult = BridgeMap.toDTO(bridgeResult) as IBridgeDTO;
        return Result.ok<IBridgeDTO>(bridgeDTOResult);
      }

    } catch (e) {
      throw e;
    }
  }

  public async updateBridge(bridgeDTO: IBridgeDTO, id:string): Promise<Result<IBridgeDTO>> {
    try {
      bridgeDTO.id  = id;
      const bridge = await this.bridgeRepo.findByDomainId(bridgeDTO.id);

      if (bridge === null) {
        return Result.fail<IBridgeDTO>("Bridge not found");
      }
      else {
        bridge.name = bridgeDTO.name;
        bridge.code = bridgeDTO.code;
        bridge.floorAId = bridgeDTO.floorAId;
        bridge.floorBId = bridgeDTO.floorBId;
        await this.bridgeRepo.save(bridge);

        const bridgeDTOResult = BridgeMap.toDTO(bridge) as IBridgeDTO;
        return Result.ok<IBridgeDTO>(bridgeDTOResult)
      }
    } catch (e) {
      throw e;
    }
  }


  public async getAllBridges(): Promise<Result<IBridgeDTO[]>> {
    try {

      const bridges = await this.bridgeRepo.getAllBridges();

      if (bridges === null) {
        return Result.fail<IBridgeDTO[]>("No bridges found");
      }
      else {
        const bridgeDTOs = bridges.map((bridges) => BridgeMap.toDTO(bridges) as IBridgeDTO);
        return Result.ok<IBridgeDTO[]>(bridgeDTOs)
      }
    } catch (e) {
      throw e;
    }
  }

  public async getBridgesBetweenBuildings(building1: string, building2: string): Promise<Result<IBridgeDTO[]>> {
    try {

      const bridges = await this.bridgeRepo.getBridgesBetweenBuildings(building1, building2);

      if (bridges === null) {
        return Result.fail<IBridgeDTO[]>("Bridge not found");
      }
      else {
        const bridgeDTOs = bridges.map((bridges) => BridgeMap.toDTO(bridges) as IBridgeDTO);
        return Result.ok<IBridgeDTO[]>(bridgeDTOs)
      }
    } catch (e) {
      throw e;
    }
  }

  public async getBridgesForBuilding(buildingId: string): Promise<Result<IBuildingBridgeDTO[]>> {
    try {
      const building = await this.buildingRepo.findByDomainId(buildingId);

      if (!building) {
        return Result.fail<IBuildingBridgeDTO[]>('Building not found');
      }

      // Vai ao repositório de bridges para buscar as passagens relacionadas com o edifício.
      const bridges = await this.bridgeRepo.getBridgesForBuilding(buildingId);

      if (bridges.length === 0) {
        return Result.fail<IBuildingBridgeDTO[]>('No bridges with passageway found for this building');
      }

      // Array para armazenar os objetos BuildingBridge
      const buildingBridges: IBuildingBridgeDTO[] = [];

      for (const bridge of bridges) {
        let floorNumber;

        if (buildingId === bridge.buildingA) {
          floorNumber = await this.floorRepo.findByDomainId(bridge.floorAId);
        } else if (buildingId === bridge.buildingB) {
          floorNumber = await this.floorRepo.findByDomainId(bridge.floorBId);
        }

        if (floorNumber) {
          const buildingBridge = BuildingBridge.create({
            buildingName: building.name,
            floorNumber: floorNumber.props.floorNumber,
            description: bridge.name
          });

          if (buildingBridge.isSuccess) {
            const buildingBridgeResult = buildingBridge.getValue();
            buildingBridges.push(buildingBridgeResult);
          } else {
            console.error(buildingBridge.error);
          }
        }
      };

      return Result.ok<IBuildingBridgeDTO[]>(buildingBridges);
    } catch (e) {
      throw e;
    }
  }
}
