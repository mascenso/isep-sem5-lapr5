import { Service, Inject } from 'typedi';
import config from "../../config";
import IBuildingRepo from './IRepos/IBuildingRepo';
import IBuildingService from "./IServices/IBuildingService";
import { IBuildingDTO } from "../dto/IBuildingDTO";
import { Result } from "../core/logic/Result";
import { BuildingMap } from "../mappers/BuildingMap";
import { Building } from "../domain/building";
import { ConnectionCheckedOutEvent } from 'mongodb';
import IBridgeRepo from './IRepos/IBridgeRepo';
import IBridgeDTO from '../dto/IBridgeDTO';
import { BridgeMap } from '../mappers/BridgeMap';
import IBuildingBridgeDTO from '../dto/IBuildingBridgeDTO';
import { BuildingBridge } from '../domain/buildingBridge';


@Service()
export default class BuildingService implements IBuildingService {
  constructor(
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
    @Inject(config.repos.bridge.name) private bridgeRepo: IBridgeRepo
  ) { }

  public async createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {

      const buildingOrError = await Building.create(buildingDTO);

      if (buildingOrError.isFailure) {
        return Result.fail<IBuildingDTO>(buildingOrError.errorValue());
      }

      const buildingResult = buildingOrError.getValue();

      await this.buildingRepo.save(buildingResult);

      const buildingDTOResult = BuildingMap.toDTO(buildingResult) as IBuildingDTO;
      return Result.ok<IBuildingDTO>(buildingDTOResult)
    } catch (e) {
      throw e;
    }
  }

  public async updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {

      let building = await this.buildingRepo.findByDomainId(buildingDTO.id);

      if (building === null) {
        return Result.fail<IBuildingDTO>('Building not found');
      }

      const fieldsToUpdate = ['name', 'description', 'maxLength', 'maxWidth', 'code'];

      for (const field of fieldsToUpdate) {
        if (buildingDTO[field]) {
          building[field] = buildingDTO[field];
        }
      }

      await this.buildingRepo.save(building);

      const buildingDTOResult = BuildingMap.toDTO(building) as IBuildingDTO;
      return Result.ok<IBuildingDTO>(buildingDTOResult);


    } catch (e) {
      throw e;
    }
  }

  public async getAllBuildings(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO[]>> {
    try {

      const buildings = await this.buildingRepo.getAllBuildings();

      const buildingDTOs = buildings.map((building) => BuildingMap.toDTO(building) as IBuildingDTO);

      return Result.ok<IBuildingDTO[]>(buildingDTOs);


    } catch (e) {
      throw e;
    }
  }

  public async getBuildingBridges(buildingId: string): Promise<Result<IBuildingBridgeDTO[]>> {
    try {
      const building = await this.buildingRepo.findByDomainId(buildingId);

      if (!building) {
        return Result.fail<IBuildingBridgeDTO[]>('Building not found');
      }

      // Vai ao repositório de bridges para buscar as passagens relacionadas com o edifício.
      const bridges = await this.bridgeRepo.getBridgesAtBuildings(buildingId, buildingId); // O metodo requer dois argumentos, passei o memso building duas vezes

      if (bridges.length === 0) {
        return Result.fail<IBuildingBridgeDTO[]>('No bridges with passageway found for this building');
      }

      // Array para armazenar os objetos BuildingBridge
      const buildingBridges: IBuildingBridgeDTO[] = [];

      bridges.forEach((bridge) => {

        const buildingBridge = BuildingBridge.create({
          buildingName: building.name,
          floorNumber: parseInt(bridge.floorA),
          description: bridge.name
        });

        if (buildingBridge.isSuccess) {
          const buildingBridgeResult = buildingBridge.getValue();
          buildingBridges.push(buildingBridgeResult);
        } else {
          console.error(buildingBridge.error);
        }
      });

      return Result.ok<IBuildingBridgeDTO[]>(buildingBridges);
    } catch (e) {
      throw e;
    }
  }

}
