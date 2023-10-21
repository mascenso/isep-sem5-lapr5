
import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import {Building} from "../domain/building";
import {IBuildingDTO} from "../dto/IBuildingDTO";
import {Document, Model} from "mongoose";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";

export class BuildingMap extends Mapper<Building> {

  public static toDTO( building: Building): IBuildingDTO {
    return {
      code: building.code,
      description: building.description,
      name: building.name,
      maxWidth: building.maxWidth,
      maxLength: building.maxLength
    } as IBuildingDTO;
  }

  public static toDomain (building: any | Model<IBuildingPersistence & Document> ): Building {
    const buildingOrError = Building.create(
      building,
      new UniqueEntityID(building.domainId)
    );

    buildingOrError.isFailure ? console.log(buildingOrError.error) : '';

    return buildingOrError.isSuccess ? buildingOrError.getValue() : null;
  }

  public static toPersistence (building: Building): any {
    const a = {
      domainId: building.id.toString(),
      code: building.code,
      description: building.description,
      name: building.name,
      maxWidth: building.maxWidth,
      maxLength: building.maxLength
    }
    return a;
  }
}
