import { Mapper } from "../core/infra/Mapper";

import { Document, Model } from 'mongoose';
import { IRobotPersistence } from '../dataschema/IRobotPersistence';

import IRobotDTO from "../dto/IRobotDTO";
import { Robot } from "../domain/robot-agg/robot";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export class RobotMap extends Mapper<Robot> {

  public static toDTO( robot: Robot): IRobotDTO {
    return {
      id: robot.id.toString(),
      nickName: robot.nickName.toString(),
      robotType: robot.robotType,
      serialNumber: robot.serialNumber,
      description: robot.description,
      inhibited: robot.inhibited
    } as IRobotDTO;
  }

  public static toDomain (robot: any | Model<IRobotPersistence & Document> ): Robot {
    const robotOrError = Robot.create({
        nickName: robot.nickName.toString(),
        robotType: robot.robotType.toString(),
        serialNumber: robot.serialNumber,
        description: robot.description,
        inhibited: robot.inhibited
      },
      new UniqueEntityID(robot.domainId)
    );

    robotOrError.isFailure ? console.log(robotOrError.error) : '';

    return robotOrError.isSuccess ? robotOrError.getValue() : null;
  }

  public static toPersistence (robot: Robot): any {
    return {
      domainId: robot.id.toString(),
      nickName: robot.nickName.toString(),
      robotType: robot.robotType.toString(),
      serialNumber: robot.serialNumber,
      description: robot.description,
      inhibited: robot.inhibited
    }
  }
}
