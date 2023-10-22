import { Service, Inject } from 'typedi';
import config from "../../config";
import IFloorRepo from './IRepos/IFloorRepo';
import IFloorService from "./IServices/IFloorService";
import {IFloorDTO} from "../dto/IFloorDTO";
import {Result} from "../core/logic/Result";
import {FloorMap} from "../mappers/FloorMap";
import {Floor} from "../domain/floor";
import IBuildingRepo from './IRepos/IBuildingRepo';
import { IBuildingDTO } from '../dto/IBuildingDTO';


@Service()
export default class FloorService implements IFloorService {
  constructor(
      @Inject(config.repos.floor.name) private floorRepo : IFloorRepo,
      @Inject(config.repos.building.name) private buildingRepo : IBuildingRepo
  ) {}

  public async createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {

      const building = await this.buildingRepo.findByDomainId(floorDTO.buildingId);

      if(building === null) {
        return Result.fail<IFloorDTO>('Building not found');
      }
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

  public async addMapToFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {
    
      const floor = await this.floorRepo.findByDomainId(floorDTO.id);
      
      if(floor === null) {
        return Result.fail<IFloorDTO>("Floor not found, you can't add a map to a floor that doesn't exist. ");
      }

      const updatedFloor = floor.addMap(floorDTO.floorMap);

      await this.floorRepo.save(updatedFloor);

      const floorDTOResult = FloorMap.toDTO( floor ) as IFloorDTO;
      return Result.ok<IFloorDTO>( floorDTOResult )
    } catch (e) {
      throw e;
    }
  }

}