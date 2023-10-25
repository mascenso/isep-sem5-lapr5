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
    @Inject('bridgeSchema') private bridgeSchema : Model<IBridgePersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(bridge: Bridge): Promise<boolean> {

    const idX = bridge.id instanceof BridgeId ? (<BridgeId>bridge.id).toValue() : bridge.id;

    const query = { domainId: idX};
    const bridgeDocument = await this.bridgeSchema.findOne( query as FilterQuery<IBridgePersistence & Document>);

    return !!bridgeDocument === true;
  }

  public async save (bridge: Bridge): Promise<Bridge> {
    const query = { domainId: bridge.id.toString()};

    const bridgeDocument = await this.bridgeSchema.findOne( query );

    try {
      if (bridgeDocument === null ) {
        const rawBridge: any = BridgeMap.toPersistence(bridge);

        const bridgeCreated = await this.bridgeSchema.create(rawBridge);

        return BridgeMap.toDomain(bridgeCreated);
      } else {

        const updateFields = [  'code', 'name' ];

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

  public async findByDomainId (bridgeId: BridgeId | string): Promise<Bridge> {
    const query = { domainId: bridgeId};
    const bridgeRecord = await this.bridgeSchema.findOne( query as FilterQuery<IBridgePersistence & Document> );

    if( bridgeRecord != null) {
      return BridgeMap.toDomain(bridgeRecord);
    }
    else
      return null;
  }

  public async getAllBridges (): Promise<any> {
    try {
      const query = {};

      const bridgeRecords = await this.bridgeSchema.find(query);

      return bridgeRecords;
    } catch (err) {
      throw err;
    }
  }

  async getBridgesAtBuildings(building1: string, building2: string): Promise<any> {
    try {
      const query = { $or: [{ floorA: building1, floorB: building2 }, { floorA: building1, floorB: building2 }] };

      //const query = {code: "123"};

      const bridgeRecords = await this.bridgeSchema.find(query);

      return bridgeRecords;

    } catch (err) {
      throw err;
    }

  }
}
