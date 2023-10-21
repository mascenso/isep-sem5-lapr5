import { Service, Inject } from 'typedi';
import config from "../../config";
import IBuildingRepo from './IRepos/IBuildingRepo';
import IBuildingService from "./IServices/IBuildingService";
import {IBuildingDTO} from "../dto/IBuildingDTO";
import {Result} from "../core/logic/Result";
import {BuildingMap} from "../mappers/BuildingMap";
import {Building} from "../domain/building";


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


}
