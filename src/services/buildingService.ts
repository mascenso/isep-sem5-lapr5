import { Service, Inject } from 'typedi';
import config from "../../config";
import IBuildingRepo from './IRepos/IBuildingRepo';
import IBuildingService from "./IServices/IBuildingService";
import {IBuildingDTO} from "../dto/IBuildingDTO";
import {Result} from "../core/logic/Result";
import {BuildingMap} from "../mappers/BuildingMap";
import {Building} from "../domain/building";
import { ConnectionCheckedOutEvent } from 'mongodb';


@Service()
export default class BuildingServiceService implements IBuildingService {
  constructor(
      @Inject(config.repos.building.name) private buildingRepo : IBuildingRepo
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

      if (buildingDTO.name) {
        building.name = buildingDTO.name;
      }
      if (buildingDTO.description) {
        building.description = buildingDTO.description;
      }
      if (buildingDTO.maxLength) {
        building.maxLength = buildingDTO.maxLength;
      }
      if (buildingDTO.maxWidth) {
        building.maxWidth = buildingDTO.maxWidth;
      }
      if (buildingDTO.code) {
        building.code = buildingDTO.code;
      }

      await this.buildingRepo.save(building);
  
      const buildingDTOResult = BuildingMap.toDTO(building) as IBuildingDTO;
      return Result.ok<IBuildingDTO>(buildingDTOResult);


    } catch (e) {
      throw e;
    }
  }


}
