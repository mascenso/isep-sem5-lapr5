import { Service, Inject } from 'typedi';

import IRobotTypeRepo from "../services/IRepos/IRobotTypeRepo";
import { RobotType } from "../domain/robotType";
import { RobotTypeId } from "../domain/robotTypeId";
import { RobotTypeMap } from "../mappers/RobotTypeMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { IRobotTypePersistence } from '../dataschema/IRobotTypePersistence';

@Service()
export default class RobotTypeRepo implements IRobotTypeRepo {
  private models: any;

  constructor(
    @Inject('robotTypeSchema') private robotTypeSchema : Model<IRobotTypePersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(robotType: RobotType): Promise<boolean> {

    const idX = robotType.id instanceof RobotTypeId ? (<RobotTypeId>robotType.id).toValue() : robotType.id;

    const query = { domainId: idX};
    const robotTypeDocument = await this.robotTypeSchema.findOne( query as FilterQuery<IRobotTypePersistence & Document>);

    return !!robotTypeDocument === true;
  }

  public async save (robotType: RobotType): Promise<RobotType> {
    const query = { domainId: robotType.id.toString()};

    const robotTypeDocument = await this.robotTypeSchema.findOne( query );

    try {
      if (robotTypeDocument === null ) {
        const rawRobotType: any = RobotTypeMap.toPersistence(robotType);

        const robotTypeCreated = await this.robotTypeSchema.create(rawRobotType);

        return RobotTypeMap.toDomain(robotTypeCreated);
      } else {
        robotTypeDocument.designacao = robotType.designacao;
        robotTypeDocument.tipoTarefas = robotType.tipoTarefas;
        await robotTypeDocument.save();

        return robotType;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (robotTypeId: RobotTypeId | string): Promise<RobotType> {
    const query = { domainId: robotTypeId};
    const robotTypeRecord = await this.robotTypeSchema.findOne( query as FilterQuery<IRobotTypePersistence & Document> );

    if( robotTypeRecord != null) {
      return RobotTypeMap.toDomain(robotTypeRecord);
    }
    else
      return null;
  }

  public async findByDesignationOrTaskType(designation: string, taskType: string): Promise<RobotType[]> {
    const query = {
      $or: [
        { designacao: designation },
        { tipoTarefas: taskType }
      ]
    };

    const robotTypeRecords = await this.robotTypeSchema.find(query);
    if (robotTypeRecords != null) {
      return robotTypeRecords.map(robotType => RobotTypeMap.toDomain(robotType));
    } else {
      return [];
    }
  }
}
