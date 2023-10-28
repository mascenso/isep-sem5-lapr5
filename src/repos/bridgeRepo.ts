import { Service, Inject } from 'typedi';

import IBridgeRepo from "../services/IRepos/IBridgeRepo";
import { Bridge } from "../domain/bridge";
import { BridgeId } from "../domain/bridgeId";
import { BridgeMap } from "../mappers/BridgeMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { IBridgePersistence } from '../dataschema/IBridgePersistence';

@Service()
export default class BridgeRepo implements IBridgeRepo {
  private models: any;

  constructor(
    @Inject('bridgeSchema') private bridgeSchema: Model<IBridgePersistence & Document>,
  ) { }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async exists(bridge: Bridge): Promise<boolean> {

    const idX = bridge.id instanceof BridgeId ? (<BridgeId>bridge.id).toValue() : bridge.id;

    const query = { domainId: idX };
    const bridgeDocument = await this.bridgeSchema.findOne(query as FilterQuery<IBridgePersistence & Document>);

    return !!bridgeDocument === true;
  }

  public async save(bridge: Bridge, buildingAId: string, buildingBId: string): Promise<Bridge> {
    const query = { domainId: bridge.id.toString() };

    const bridgeDocument = await this.bridgeSchema.findOne(query);

    try {
      if (bridgeDocument === null) {
        const rawBridge: any = BridgeMap.toPersistence(bridge, buildingAId, buildingBId);

        const bridgeCreated = await this.bridgeSchema.create(rawBridge);

        return BridgeMap.toDomain(bridgeCreated);
      } else {

        const updateFields = ['code', 'name'];

        for (const field of updateFields) {
          if (bridge[field] !== undefined) {
            bridgeDocument[field] = bridge[field];
          }
        }

        await bridgeDocument.save();
        return bridge;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId(bridgeId: BridgeId | string): Promise<Bridge> {
    const query = { domainId: bridgeId };
    const bridgeRecord = await this.bridgeSchema.findOne(query as FilterQuery<IBridgePersistence & Document>);

    if (bridgeRecord != null) {
      return BridgeMap.toDomain(bridgeRecord);
    }
    else
      return null;
  }

  public async getAllBridges(): Promise<any> {
    try {
      const query = {};

      const bridgeRecords = await this.bridgeSchema.find(query);

      return bridgeRecords;
    } catch (err) {
      throw err;
    }
  }

  async getBridgesBetweenBuildings(building1: string, building2: string): Promise<any> {
    try {

      console.log("getBridgesBetweenBuildings: " + building1 + " " + building2);
      "guardava uma vez apenas na BD e a pesquisar pesquisava das 2 formas."
      const query = { $or: [{ buildingA: building1, buildingB: building2 }, { buildingA: building2, buildingB: building1 }] };

      const bridgeRecords = await this.bridgeSchema.find(query);

      return bridgeRecords;

    } catch (err) {
      throw err;
    }
  }

  async areConnected(floorA: string, floorB: string): Promise<boolean> {
    try {
      const query = { $or: [{ floorA: floorA, floorB: floorB }, { floorA: floorB, floorB: floorA }] };

      const bridgeRecords = await this.bridgeSchema.findOne(query);

      return bridgeRecords != null;

    } catch (err) {
      throw err;
    }
  }

  async getBridgesForBuilding(buildingId: string): Promise<any> {
    try {
      const query = {
        $or: [
          { buildingA: buildingId },
          { buildingB: buildingId }
        ]
      };

      const bridgeRecords = await this.bridgeSchema.find(query);
      
      return bridgeRecords;

    } catch (err) {
      throw err;
    }
  }
}
