
import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import {Floor} from "../domain/floor";
import {IFloorDTO} from "../dto/IFloorDTO";
import {Document, Model} from "mongoose";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";

export class FloorMap extends Mapper<Floor> {

  public static toDTO( floor: Floor): IFloorDTO {
    return {
      id: floor.id.toString(),
      buildingId: floor.buildingId,
      description: floor.description,
      floorNumber: floor.floorNumber,
      width: floor.width,
      length: floor.length,
      floorMap: floor.floorMap
    } as IFloorDTO;
  }

  public static toDomain (floor: any | Model<IFloorPersistence & Document> ): Floor {
    const floorOrError = Floor.create(
      floor,
      new UniqueEntityID(floor.domainId)
    );

    floorOrError.isFailure ? console.log(floorOrError.error) : '';

    return floorOrError.isSuccess ? floorOrError.getValue() : null;
  }

  public static toPersistence (floor: Floor): any {
    const a = {
      domainId: floor.id.toString(),
      buildingId: floor.buildingId,
      description: floor.description,
      floorNumber: floor.floorNumber,
      width: floor.width,
      length: floor.length,
      floorMap: floor.floorMap
    }
    return a;
  }
}