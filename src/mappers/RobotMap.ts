import { Mapper } from "../core/infra/Mapper";

import { Document, Model } from 'mongoose';
import { IRobotPersistence } from '../dataschema/IRobotPersistence';

import IRobotDTO from "../dto/IRobotDTO";
import { Robot } from "../domain/robot";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export class RobotMap extends Mapper<Robot> {
  
  public static toDTO( robot: Robot): IRobotDTO {
    return {
      id: robot.id.toString(),
      designacao: robot.designacao,
      tarefas: robot.tarefas,
    } as IRobotDTO;
  }

  public static toDomain (robot: any | Model<IRobotPersistence & Document> ): Robot {
    const robotOrError = Robot.create(
      robot,
      new UniqueEntityID(robot.domainId)
    );

    robotOrError.isFailure ? console.log(robotOrError.error) : '';

    return robotOrError.isSuccess ? robotOrError.getValue() : null;
  }

  public static toPersistence (robot: Robot): any {
    return {
      domainId: robot.id.toString(),
      designacao: robot.designacao,
      tarefas: robot.tarefas
    }
  }
}