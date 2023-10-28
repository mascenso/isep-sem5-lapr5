import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';
import IBuildingRepo from "../services/IRepos/IBuildingRepo";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";
import {BuildingId} from "../domain/buildingId";
import {BuildingMap} from "../mappers/BuildingMap";
import {Building} from "../domain/building";

@Service()
export default class BuildingRepo implements IBuildingRepo {
  private models: any;

  constructor(
    @Inject('buildingSchema') private buildingSchema : Model<IBuildingPersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(building: Building): Promise<boolean> {

    const idX = building.id instanceof BuildingId ? (<BuildingId>building.id).toValue() : building.id;

    const query = { domainId: idX};
    const buildingDocument = await this.buildingSchema.findOne( query as FilterQuery<IBuildingPersistence & Document>);

    return !!buildingDocument === true;
  }

  public async save (building: Building): Promise<Building> {
    const query = { domainId: building.id.toString()};

    const buildingDocument = await this.buildingSchema.findOne( query );

    try {
      if (buildingDocument === null ) {
        const rawBuilding: any = BuildingMap.toPersistence(building);

        const buildingCreated = await this.buildingSchema.create(rawBuilding);

        return BuildingMap.toDomain(buildingCreated);
      } else {

        const updateFields = ['name', 'code', 'description', 'maxWidth', 'maxLength'];

        for (const field of updateFields) {
          if (building[field] !== undefined) {
            buildingDocument[field] = building[field];
          }
        }

        await buildingDocument.save();
        return building;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (buildingId: BuildingId | string): Promise<Building> {
    const query = { domainId: buildingId};

    const buildingRecord = await this.buildingSchema.findOne( query as FilterQuery<IBuildingPersistence & Document> );

    if( buildingRecord != null) {
      return BuildingMap.toDomain(buildingRecord);
    }
    else
      return null;
  }

  public async getAllBuildings (): Promise<Building[]> {
    try {
      const query = {};
      const buildings = await this.buildingSchema.find(query);
      return buildings.map(building => BuildingMap.toDomain(building));
    } catch (err) {
      throw err;
    }
  }

}
