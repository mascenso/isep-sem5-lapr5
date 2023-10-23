import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import {Elevator} from "../domain/elevator";
import {IElevatorDTO} from "../dto/IElevatorDTO";
import {Document, Model} from "mongoose";
import {IElevatorPersistence} from "../dataschema/IElevatorPersistence";

export class ElevatorMap extends Mapper<Elevator> {

  public static toDTO( elevator: Elevator): IElevatorDTO {
    return {
      code: elevator.code,
      coordX: elevator.coordX,
      coordY: elevator.coordY
    } as IElevatorDTO;
  }

  public static toDomain (elevator: any | Model<IElevatorPersistence & Document> ): Elevator {

    const elevatorOrError = Elevator.create(
        elevator,
      new UniqueEntityID(elevator.domainId)
    );
      
    elevatorOrError.isFailure ? console.log(elevatorOrError.error) : '';

    return elevatorOrError.isSuccess ? elevatorOrError.getValue() : null;
  }

  public static toPersistence (elevator: Elevator): any {
    const a = {
      domainId: elevator.id.toString(),
      code: elevator.code,
      coordX: elevator.coordX,
      coordY: elevator.coordY
    }
    return a;
  }
}
