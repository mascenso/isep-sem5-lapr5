import { Service, Inject } from 'typedi';
import config from "../../config";
import IBuildingRepo from './IRepos/IBuildingRepo';
import IBuildingService from "./IServices/IBuildingService";
import {IBuildingDTO} from "../dto/IBuildingDTO";
import {Result} from "../core/logic/Result";
import {BuildingMap} from "../mappers/BuildingMap";
import {Building} from "../domain/building";
import {BuildingId} from "../domain/buildingId";


@Service()
export default class BuildingService implements IBuildingService {
  constructor(
      @Inject(config.repos.building.name) private buildingRepo : IBuildingRepo,
  ) {}

  public async createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {

      const buildingOrError = await Building.create( buildingDTO );

      if (buildingOrError.isFailure) {
        return Result.fail<IBuildingDTO>(buildingOrError.errorValue());
      }

      const buildingResult = buildingOrError.getValue();

      await this.buildingRepo.save(buildingResult);

      const buildingDTOResult = BuildingMap.toDTO( buildingResult ) as IBuildingDTO;

      return Result.ok<IBuildingDTO>( buildingDTOResult )
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

  public async getAllBuildings(): Promise<Result<IBuildingDTO[]>> {
    try {

      const buildings = await this.buildingRepo.getAllBuildings();

      const buildingDTOs = buildings.map((building) => BuildingMap.toDTO(building) as IBuildingDTO);
      return Result.ok<IBuildingDTO[]>(buildingDTOs);

    } catch (e) {
      throw e;
    }
  }

  public async getBuildingById(buildingId: BuildingId | string): Promise<Result<IBuildingDTO>> {
    try {

      let building = await this.buildingRepo.findByDomainId(buildingId);

      if (building === null) {
        return Result.fail<IBuildingDTO>('Building not found');
      }
      return Result.ok<IBuildingDTO>(BuildingMap.toDTO(building));

    } catch (e) {
      throw e;
    }
  }

}
