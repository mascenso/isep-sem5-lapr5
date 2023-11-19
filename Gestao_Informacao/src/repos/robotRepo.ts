import { Service, Inject } from 'typedi';

import IRobotRepo from "../services/IRepos/IRobotRepo";
import { Robot } from "../domain/robot-agg/robot";
import { RobotId } from "../domain/robot-agg/robotId";
import { RobotMap } from "../mappers/RobotMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { IRobotPersistence } from '../dataschema/IRobotPersistence';

@Service()
export default class RobotRepo implements IRobotRepo {
  private models: any;

  constructor(
    @Inject('robotSchema') private robotSchema : Model<IRobotPersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(robot: Robot): Promise<boolean> {

    const idX = robot.id instanceof RobotId ? (<RobotId>robot.id).toValue() : robot.id;

    const query = { domainId: idX};
    const robotDocument = await this.robotSchema.findOne( query as FilterQuery<IRobotPersistence & Document>);

    return !!robotDocument === true;
  }

  public async save (robot: Robot): Promise<Robot> {
    const query = { domainId: robot.id.toString()};

    const robotDocument = await this.robotSchema.findOne( query );

    try {
      if (robotDocument === null ) {
        const rawRobot: any = RobotMap.toPersistence(robot);

        const robotCreated = await this.robotSchema.create(rawRobot);

        return RobotMap.toDomain(robotCreated);
      } else {
        robotDocument.nickName = robot.nickName;
        robotDocument.robotType = robot.robotType;
        robotDocument.serialNumber = robot.serialNumber;
        robotDocument.description = robot.description;
        robotDocument.inhibited = robot.inhibited;
        await robotDocument.save();

        return robot;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (robotId: RobotId | string): Promise<Robot> {
    const query = { domainId: robotId};
    const robotRecord = await this.robotSchema.findOne( query as FilterQuery<IRobotPersistence & Document> );

    if( robotRecord != null) {
      return RobotMap.toDomain(robotRecord);
    }
    else
      return null;
  }

  public async getAllRobots (): Promise<any> {
    try {
      const query = {};

      const robotRecords = await this.robotSchema.find(query);

      return robotRecords;
    } catch (err) {
      throw err;
    }
  }

  public async findByDesignationOrTaskType(designation: string, robotTypeList: string[]): Promise<Robot[]> {
    let query;
    if (designation && robotTypeList.length === 0) {
      query = {nickName: designation };
    }
    else if (robotTypeList.length != 0 && designation) {
      query = {
        $and: [
              { nickName: designation },
              { robotType: {$in: robotTypeList} },
            ]
      };
    }
    else if ( robotTypeList.length != 0 && designation === null) {
      query = {robotType: {$in: robotTypeList} };
    }
    else {
     query = {};
    }

    const robotRecords = await this.robotSchema.find(query);
    if (robotRecords != null) {
      return robotRecords.map(robot => RobotMap.toDomain(robot));
    } else {
      return [];
    }
  }
}
