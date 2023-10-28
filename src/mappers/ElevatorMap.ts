import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import {Elevator} from "../domain/elevator";
import {IElevatorDTO} from "../dto/IElevatorDTO";
import {Document, Model} from "mongoose";
import {IElevatorPersistence} from "../dataschema/IElevatorPersistence";

export class ElevatorMap extends Mapper<Elevator> {

  public static toDTO( elevator: Elevator): IElevatorDTO {
    return {
      id:elevator.id.toString(),
      code: elevator.code,
      floorId: elevator.floorId,
      coordX1: elevator.coordX1,
      coordY1: elevator.coordY1,
      coordX2: elevator.coordX2,
      coordY2: elevator.coordY2
    } as IElevatorDTO;
  }

  public static toDomain (elevator: any | Model<IElevatorPersistence & Document> ): Elevator {

    const elevatorOrError = Elevator.create({
        code: elevator.code,
        floorId: elevator.floorId,
        coordX1: elevator.coordX1,
        coordY1: elevator.coordY1,
        coordX2: elevator.coordX2,
        coordY2: elevator.coordY2
      },
      new UniqueEntityID(elevator.domainId)
    );

    elevatorOrError.isFailure ? console.log(elevatorOrError.error) : '';

    return elevatorOrError.isSuccess ? elevatorOrError.getValue() : null;
  }

  public static toPersistence (elevator: Elevator): any {
    const a = {
      domainId: elevator.id.toString(),
      code: elevator.code,
      floorId: elevator.floorId,
      coordX1: elevator.coordX1,
      coordY1: elevator.coordY1,
      coordX2: elevator.coordX2,
      coordY2: elevator.coordY2
    }
    return a;
  }
}
