import { Service, Inject } from 'typedi';
import config from "../../config";
import IFloorRepo from './IRepos/IFloorRepo';
import IFloorService from "./IServices/IFloorService";
import {IFloorDTO} from "../dto/IFloorDTO";
import {Result} from "../core/logic/Result";
import {FloorMap} from "../mappers/FloorMap";
import {Floor} from "../domain/floor";


@Service()
export default class FloorService implements IFloorService {
  constructor(
      @Inject(config.repos.floor.name) private floorRepo : IFloorRepo
  ) {}

  public async createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {

      const floorOrError = await Floor.create( floorDTO );

      if (floorOrError.isFailure) {
        return Result.fail<IFloorDTO>(floorOrError.errorValue());
      }

      const floorResult = floorOrError.getValue();

      await this.floorRepo.save(floorResult);

      const floorDTOResult = FloorMap.toDTO( floorResult ) as IFloorDTO;
      return Result.ok<IFloorDTO>( floorDTOResult )
    } catch (e) {
      throw e;
    }
  }

}