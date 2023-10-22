import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';
import IFloorRepo from "../services/IRepos/IFloorRepo";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";
import {FloorId} from "../domain/floorId";
import {FloorMap} from "../mappers/FloorMap";
import {Floor} from "../domain/floor";

@Service()
export default class FloorRepo implements IFloorRepo {
  private models: any;

  constructor(
    @Inject('floorSchema') private floorSchema : Model<IFloorPersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(floor: Floor): Promise<boolean> {

    const idX = floor.id instanceof FloorId ? (<FloorId>floor.id).toValue() : floor.id;

    const query = { domainId: idX};
    const floorDocument = await this.floorSchema.findOne( query as FilterQuery<IFloorPersistence & Document>);

    return !!floorDocument === true;
  }

  public async save (floor: Floor): Promise<Floor> {
    const query = { domainId: floor.id.toString()};

    const floorDocument = await this.floorSchema.findOne( query );

    try {
      if (floorDocument === null ) {
        const rawFloor: any = FloorMap.toPersistence(floor);

        const floorCreated = await this.floorSchema.create(rawFloor);

        return FloorMap.toDomain(floorCreated);
      } else {

        const fieldsToUpdate = ['floorNumber', 'buildingId', 'description', 'width', 'length', 'floorMap'];

        for (const field of fieldsToUpdate) {
          if (floor[field] !== undefined) {
            floorDocument[field] = floor[field];
          }
        }

        await floorDocument.save();
        return floor;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (floorId: FloorId | string): Promise<Floor> {
    const query = { domainId: floorId};

    const floorRecord = await this.floorSchema.findOne( query as FilterQuery<IFloorPersistence & Document> );
    
    if( floorRecord != null) {
      return FloorMap.toDomain(floorRecord);
    }
    else
      return null;
  }
}