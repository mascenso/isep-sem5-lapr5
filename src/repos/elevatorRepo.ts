import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';
import IElevatorRepo from "../services/IRepos/IElevatorRepo";
import {IElevatorPersistence} from "../dataschema/IElevatorPersistence";
import {ElevatorId} from "../domain/elevator-agg/elevatorId";
import {ElevatorMap} from "../mappers/ElevatorMap";
import {Elevator} from "../domain/elevator-agg/elevator";
import {FloorId} from "../domain/floorId";

@Service()
export default class ElevatorRepo implements IElevatorRepo {
  private models: any;

  constructor(
    @Inject('elevatorSchema') private elevatorSchema : Model<IElevatorPersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(elevator: Elevator): Promise<boolean> {

    const idX = elevator.id instanceof ElevatorId ? (<ElevatorId>elevator.id).toValue() : elevator.id;

    const query = { domainId: idX};
    const elevatorDocument = await this.elevatorSchema.findOne( query as FilterQuery<IElevatorPersistence & Document>);

    return !!elevatorDocument === true;
  }

  public async save (elevator: Elevator): Promise<Elevator> {
    const query = { domainId: elevator.id.toString()};

    const elevatorDocument = await this.elevatorSchema.findOne( query );

    try {
      if (elevatorDocument === null ) {
        const rawElevator: any = ElevatorMap.toPersistence(elevator);

        const elevatorCreated = await this.elevatorSchema.create(rawElevator);

        return ElevatorMap.toDomain(elevatorCreated);
      } else {

        const updateFields = ['code', 'coordX1', 'coordY1', 'coordX2', 'coordY2'];

        for (const field of updateFields) {
          if (elevator[field] !== undefined) {
            elevatorDocument[field] = elevator[field];
          }
        }

        await elevatorDocument.save();
        return elevator;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (elevatorId: ElevatorId | string): Promise<Elevator> {
    const query = { domainId: elevatorId};

    const elevatorRecord = await this.elevatorSchema.findOne( query as FilterQuery<IElevatorPersistence & Document> );

    if( elevatorRecord != null) {
      return ElevatorMap.toDomain(elevatorRecord);
    }
    else
      return null;
  }

  public async getAllElevators (): Promise<any> {
    try {
      const query = {};

      const elevatorRecords = await this.elevatorSchema.find(query);

      return elevatorRecords;
    } catch (err) {
      throw err;
    }
  }

  public async findByFloorIds(floorIds: FloorId[] | string[]): Promise<Elevator[]> {
    const query = {
      floorId: {$in: floorIds}
    };

    const elevatorRecords = await this.elevatorSchema.find(query);

    if( elevatorRecords != null) {
      return elevatorRecords.map(elevator => ElevatorMap.toDomain(elevator));
    }
    else {
      return null;
    }

  }

}
