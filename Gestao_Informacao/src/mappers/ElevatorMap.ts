import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import {Elevator} from "../domain/elevator-agg/elevator";
import {IElevatorDTO} from "../dto/IElevatorDTO";
import {Document, Model} from "mongoose";
import {IElevatorPersistence} from "../dataschema/IElevatorPersistence";

export class ElevatorMap extends Mapper<Elevator> {

  public static toDTO( elevator: Elevator): IElevatorDTO {
    return {
      id:elevator.id.toString(),
      code: elevator.code,
      floorList: elevator.floorList,
      buildingId: elevator.buildingId
    } as IElevatorDTO;
  }

  public static toDomain (elevator: any | Model<IElevatorPersistence & Document> ): Elevator {

    const elevatorOrError = Elevator.create({
        code: elevator.code,
        floorList: elevator.floorList,
        buildingId: elevator.buildingId
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
      floorList: elevator.floorList,
      buildingId: elevator.buildingId
    }
    return a;
  }
}
