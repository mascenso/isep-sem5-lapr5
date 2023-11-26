import { Service, Inject } from 'typedi';
import config from "../../config";
import IElevatorRepo from './IRepos/IElevatorRepo';
import IElevatorService from "./IServices/IElevatorService";
import {IElevatorDTO} from "../dto/IElevatorDTO";
import {Result} from "../core/logic/Result";
import {ElevatorMap} from "../mappers/ElevatorMap";
import {Elevator} from "../domain/elevator-agg/elevator";
import IBuildingRepo from './IRepos/IBuildingRepo';
import IFloorRepo from './IRepos/IFloorRepo';
import { IBuildingDTO } from '../dto/IBuildingDTO';


@Service()
export default class ElevatorService implements IElevatorService {
  constructor(
      @Inject(config.repos.elevator.name) private elevatorRepo : IElevatorRepo,
      @Inject(config.repos.building.name) private buildingRepo : IBuildingRepo,
      @Inject(config.repos.floor.name) private floorRepo : IFloorRepo
  ) {}

  public async createElevator(elevatorDTO: IElevatorDTO): Promise<Result<IElevatorDTO>> {
    try {

      const building = await this.buildingRepo.findByDomainId(elevatorDTO.buildingId);

      if(building === null) {
        return Result.fail<IElevatorDTO>('Building not found');
      }

      for (const floorId of elevatorDTO.floorList) {
        const floor = await this.floorRepo.findByDomainId(floorId);

        if(floor === null) {
          return Result.fail<IElevatorDTO>('Floor not found');
        }
      }

      const elevatorOrError = await Elevator.create( elevatorDTO );

      if (elevatorOrError.isFailure) {
        return Result.fail<IElevatorDTO>(elevatorOrError.errorValue());
      }

      const elevatorResult = elevatorOrError.getValue();

      await this.elevatorRepo.save(elevatorResult);

      const elevatorDTOResult = ElevatorMap.toDTO( elevatorResult ) as IElevatorDTO;
      return Result.ok<IElevatorDTO>( elevatorDTOResult )
    } catch (e) {
      throw e;
    }
  }

  public async updateElevator(elevatorDTO: IElevatorDTO): Promise<Result<IElevatorDTO>> {
    try {
   
      let elevator = await this.elevatorRepo.findByDomainId(elevatorDTO.id);
      if (elevator === null) {
        return Result.fail<IElevatorDTO>('Elevator with that ID does not exist.');
      }

      const fieldsToUpdate = ['code', 'floorList'];

      for (const field of fieldsToUpdate) {
        if (elevatorDTO[field]) {
            elevator[field] = elevatorDTO[field];
        }
      }

      await this.elevatorRepo.save(elevator);

      const elevatorDTOResult = ElevatorMap.toDTO(elevator) as IElevatorDTO;
      return Result.ok<IElevatorDTO>(elevatorDTOResult);


    } catch (e) {
      throw e;
    }
  }

  public async getAllElevators(): Promise<Result<IElevatorDTO[]>> {
    try {

      const elevators = await this.elevatorRepo.getAllElevators();

      const elevatorDTOs = elevators.map((elevator) => ElevatorMap.toDTO(elevator) as IElevatorDTO);

      return Result.ok<IElevatorDTO[]>(elevatorDTOs);


    } catch (e) {
      throw e;
    }
  }

  
  public async getBuildingElevators(buildingId: string): Promise<Result<IElevatorDTO>> {
    try {

      const elevator = await this.elevatorRepo.findByBuildingId(buildingId);

      if (elevator == null) {
        return Result.fail<IElevatorDTO>(`Building with id ${buildingId} doesn't have any elevator!`);
      }

      const elevatorDTOs=ElevatorMap.toDTO(elevator);

      return Result.ok<IElevatorDTO>(elevatorDTOs);
    } catch (e) {
      throw e;
    }
  }
  
}
