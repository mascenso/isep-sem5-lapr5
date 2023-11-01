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
import { IFloorDTO } from "../dto/IFloorDTO";
import { FloorMap } from "../mappers/FloorMap";

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
      // Vai ao repositório de bridges para buscar as passagens relacionadas com o edifício.
      const bridgesDoc = await this.bridgeRepo.getBridgesForBuilding(buildingId);
      const bridgeDTOs = bridgesDoc.map((bridges) => BridgeMap.toDTO(bridges) as IBridgeDTO);
      if (bridgeDTOs === null) {
        return Result.fail<null>("No bridges with passageway found for this building");
      }

      // Mapa de Piso --> [Lista de Pisos] para construir os BuildingBridgeDTO, em O(n) já agora
      const mapaBuildingBridges = new Map<string, IFloorDTO[]>();

      for (const bridgeDTO of bridgeDTOs) {

            const floorObjA = await this.floorRepo.findByDomainId(bridgeDTO.floorAId);
            const floorObjB = await this.floorRepo.findByDomainId(bridgeDTO.floorBId);
            let floorDTOA;
            let floorDTOB;


            if (floorObjA == null || floorObjB == null) {
              return Result.fail<null>(`ERR: Shouldn't get here`);
            }
            else {

              if (floorObjA.buildingId == buildingId) {
                floorDTOA = FloorMap.toDTO(floorObjA) as IFloorDTO;
                floorDTOB = FloorMap.toDTO(floorObjB) as IFloorDTO;
              } else {
                floorDTOA = FloorMap.toDTO(floorObjB) as IFloorDTO;
                floorDTOB = FloorMap.toDTO(floorObjA) as IFloorDTO;
              }

              // Just because JS doesn't allow to use objects as keys unlike Java
              // we stringify and then parse again to floorDTO
              const key  = JSON.stringify(floorDTOA);

            if (mapaBuildingBridges.has(key)) {
              const floors = mapaBuildingBridges.get(key);
              floors.push(floorDTOB);
              mapaBuildingBridges.set(key, floors);
            }
            else {
                const floors: IFloorDTO[] = [];
                floors.push(floorDTOB);
                mapaBuildingBridges.set(key, floors);
            }
          }
      }

      const floorRelationshipDTOs: IBuildingBridgeDTO[] = [];
      mapaBuildingBridges.forEach((connectedFloors, floor) => {
        const floorKeyDTO = JSON.parse(floor) as IFloorDTO;
        const floorRelationshipDTO = { floor: floorKeyDTO, connectedFloors: connectedFloors };
        floorRelationshipDTOs.push(floorRelationshipDTO);
      });

      return Result.ok<IBuildingBridgeDTO[]>(floorRelationshipDTOs);
    } catch (e) {
      throw e;
      }
  }
}
