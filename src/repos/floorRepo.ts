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

  async getFloorsAtBuildings(building: string): Promise<any> {
    try {
      const query = { buildingId: building };

      const floorRecords = await this.floorSchema.find(query);

      return floorRecords;

    } catch (err) {
      throw err;
    }
  }

  async getAllFloors(): Promise<any> {
    try {
      const floorRecords = await this.floorSchema.find();

      return floorRecords;

    } catch (err) {
      throw err;
    }
  }

  public async getBuildingsByMinMaxFloors (minFloors: number, maxFloors: number): Promise<any> {
    try {
      console.log("isto é chamado?");
      const allFloors = await this.floorSchema.find({});

      const floorsGroupedByBuildingId = {};
      allFloors.forEach(floor => {
        if (!floorsGroupedByBuildingId[floor.buildingId]) {
          floorsGroupedByBuildingId[floor.buildingId] = [];
        }
      floorsGroupedByBuildingId[floor.buildingId].push(floor);
      });
      console.log("teste: %s", floorsGroupedByBuildingId);
      const filteredBuildings = [];
      for (const buildingId in floorsGroupedByBuildingId) {
        const floors = floorsGroupedByBuildingId[buildingId];
        const numberOfFloors = floors.length;
        if (numberOfFloors >= minFloors && numberOfFloors <= maxFloors) {
          const buildingInfo = buildingId;
          /*
          const buildingInfo = {
            buildingId: buildingId,
            floors: floors
          };
          */
          
          filteredBuildings.push(buildingInfo);
        }
      }
      console.log("fdasdasdas:", JSON.stringify(filteredBuildings, null, 2));

      return filteredBuildings;
      } catch (err) {
        throw err;
      }
  }
}

