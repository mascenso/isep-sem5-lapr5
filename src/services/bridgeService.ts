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

      const floorA = await this.floorRepo.findByDomainId(bridgeDTO.floorA);
      const floorB = await this.floorRepo.findByDomainId(bridgeDTO.floorB);
      if (floorA === null || floorB === null) {
        return Result.fail<IBridgeDTO>('Floor not found');
      }

      const buildingAId = floorA.buildingId;
      const buildingBId = floorB.buildingId;

      const bridgeOrError = await Bridge.create(bridgeDTO);

      if (bridgeOrError.isFailure) {
        return Result.fail<IBridgeDTO>(bridgeOrError.errorValue());
      }

      if (await this.bridgeRepo.areConnected(bridgeDTO.floorA, bridgeDTO.floorB))
      // Combinação já existente
      {
        return Result.fail<IBridgeDTO>('Bridge already exists');
      }
      /*
      else if (buildingAId === buildingBId)
       // Nao podem estar no mesmo building
      {
        return Result.fail<IBridgeDTO>('Bridge cannot connect floors of the same building');
      }
      */
      else {
        // Criação e persistência do novo objeto Bridge
        const bridgeResult = bridgeOrError.getValue();
        await this.bridgeRepo.save(bridgeResult, buildingAId, buildingBId);

        const bridgeDTOResult = BridgeMap.toDTO(bridgeResult, buildingAId, buildingBId) as IBridgeDTO;
        return Result.ok<IBridgeDTO>(bridgeDTOResult)
      }

    } catch (e) {
      throw e;
    }
  }

  public async updateBridge(bridgeDTO: IBridgeDTO): Promise<Result<IBridgeDTO>> {
    try {
      const bridge = await this.bridgeRepo.findByDomainId(bridgeDTO.id);

      if (bridge === null) {
        return Result.fail<IBridgeDTO>("Bridge not found");
      }
      else {
        bridge.name = bridgeDTO.name;
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

  public async getBridgesAtBuildings(building1: string, building2: string): Promise<Result<IBridgeDTO[]>> {
    try {

      const bridges = await this.bridgeRepo.getBridgesAtBuildings(building1, building2);

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
          floorNumber = await this.floorRepo.findByDomainId(bridge.floorA);
        } else if (buildingId === bridge.buildingB) {
          floorNumber = await this.floorRepo.findByDomainId(bridge.floorB);
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
