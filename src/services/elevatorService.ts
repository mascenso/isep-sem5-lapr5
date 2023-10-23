import { Service, Inject } from 'typedi';
import config from "../../config";
import IElevatorRepo from './IRepos/IElevatorRepo';
import IElevatorService from "./IServices/IElevatorService";
import {IElevatorDTO} from "../dto/IElevatorDTO";
import {Result} from "../core/logic/Result";
import {ElevatorMap} from "../mappers/ElevatorMap";
import {Elevator} from "../domain/elevator";


@Service()
export default class ElevatorService implements IElevatorService {
  constructor(
      @Inject(config.repos.elevator.name) private elevatorRepo : IElevatorRepo
  ) {}

  public async createElevator(elevatorDTO: IElevatorDTO): Promise<Result<IElevatorDTO>> {
    try {

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
/*
  public async updateElevator(elevatorDTO: IElevatorDTO): Promise<Result<IElevatorDTO>> {
    try {

      let elevator = await this.elevatorRepo.findByDomainId(elevatorDTO.id);

      if (elevator === null) {
        return Result.fail<IElevatorDTO>('Elevator not found');
      }

      const fieldsToUpdate = ['code', 'coordX', 'coordY'];

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

  */
  public async getAllElevators(elevatorDTO: IElevatorDTO): Promise<Result<IElevatorDTO[]>> {
    try {

      const elevators = await this.elevatorRepo.getAllElevators();

      const elevatorDTOs = elevators.map((elevator) => ElevatorMap.toDTO(elevator) as IElevatorDTO);
  
      return Result.ok<IElevatorDTO[]>(elevatorDTOs);


    } catch (e) {
      throw e;
    }
  }


}
