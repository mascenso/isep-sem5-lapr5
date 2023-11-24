import { Service, Inject } from 'typedi';
import config from "../../config";
import IFloorRepo from './IRepos/IFloorRepo';
import IFloorService from "./IServices/IFloorService";
import {IFloorDTO} from "../dto/IFloorDTO";
import {Result} from "../core/logic/Result";
import {FloorMap} from "../mappers/FloorMap";
import {Floor} from "../domain/floor-agg/floor";
import IBuildingRepo from './IRepos/IBuildingRepo';
import { IBuildingDTO } from '../dto/IBuildingDTO';
import { BuildingMap } from '../mappers/BuildingMap';
import IElevatorRepo from "./IRepos/IElevatorRepo";
import {floor} from "lodash";


@Service()
export default class FloorService implements IFloorService {
  constructor(
      @Inject(config.repos.building.name) private buildingRepo : IBuildingRepo,
      @Inject(config.repos.floor.name) private floorRepo : IFloorRepo,
      @Inject(config.repos.elevator.name) private elevatorRepo : IElevatorRepo
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

  public async getAllFloors(): Promise<Result<IFloorDTO[]>> {
    try {

      const floor = await this.floorRepo.getAllFloors();

      if (floor === null) {
        return Result.fail<IFloorDTO[]>("No floors found.");
      }
      else {
        const floorDTOs = floor.map((floor) => FloorMap.toDTO(floor) as IFloorDTO);
        return Result.ok<IFloorDTO[]>( floorDTOs)
      }
    } catch (e) {
      throw e;
    }
  }

  public async addMapToFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    console.log("cheguei")
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

  public async updateFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {

      let floor = await this.floorRepo.findByDomainId(floorDTO.id);

      if (floor === null) {
        return Result.fail<IFloorDTO>('Floor not found');
      }

      const fieldsToUpdate = ['width', 'length', 'floorNumber', 'description', 'floorMap'];

      for (const field of fieldsToUpdate) {
        if (floorDTO[field]) {
          floor[field] = floorDTO[field];
        }
      }

      await this.floorRepo.save(floor);

      const floorDTOResult = FloorMap.toDTO(floor) as IFloorDTO;
      return Result.ok<IFloorDTO>(floorDTOResult);


    } catch (e) {
      throw e;
    }
  }

  public async getFloorsAtBuildings(building: string): Promise<Result<IFloorDTO[]>> {
    try {

      const floors = await this.floorRepo.getFloorsAtBuildings(building);

      if (floors === null || floors.length === 0) {
        return Result.fail<IFloorDTO[]>("There's currently no floors on that building.");
      }
      else {
        const floorDTOs = floors.map((floors) => FloorMap.toDTO(floors) as IFloorDTO);
        return Result.ok<IFloorDTO[]>( floorDTOs)
      }
    } catch (e) {
      throw e;
    }
  }

  public async getBuildingsByMinMaxFloors(minFloors: number, maxFloors: number): Promise<Result<IBuildingDTO[]>> {
    try {
      const buildingsByFloor = await this.floorRepo.getBuildingsByMinMaxFloors(minFloors, maxFloors);
      const buildings = await this.buildingRepo.findByDomainIds(buildingsByFloor);
      const buildingDTOs = buildings.map(building => BuildingMap.toDTO(building) as IBuildingDTO);
      return Result.ok<IBuildingDTO[]>(buildingDTOs);
    } catch (e) {
      throw e;
    }
  }

  public async getFloorsWithElevatorByBuildingId(buildingId: string): Promise<Result<IFloorDTO[]>> {
    try {
      // get elevator by building Id
      const elevator = await this.elevatorRepo.findByBuildingId(buildingId);
      if (elevator == null) {
        return Result.fail<IFloorDTO[]>(`Building with id ${buildingId} doesn't have any elevator!`);
      }

      // get floors using the list in the elevator

      const floorListOrError = await this.floorRepo.findFloorsByListOfIds(elevator.floorList);
      if (floorListOrError == null || floorListOrError.length === 0) {
        return Result.fail<IFloorDTO[]>(`Elevator with id ${elevator.id} doesn't serve any floor!`);
      }

      // take the list of elevators, transform it in a list of floorIDs,
      // filter the original floorDTO list by floorId
      const floorDTOListResult = floorListOrError
        .flatMap(floor => FloorMap.toDTO(floor));

      return Result.ok<IFloorDTO[]>(floorDTOListResult);

    } catch (e) {
      throw e;
    }
  }

}
