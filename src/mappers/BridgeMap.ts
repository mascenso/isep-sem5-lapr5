import { Mapper } from "../core/infra/Mapper";

import { Document, Model } from 'mongoose';
import { IBridgePersistence } from '../dataschema/IBridgePersistence';

import IBridgeDTO from "../dto/IBridgeDTO";
import { Bridge } from "../domain/bridge";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";


export class BridgeMap extends Mapper<Bridge> {

  public static toDTO( bridge: Bridge): IBridgeDTO {
    return {
      id: bridge.id.toString(),
      code: bridge.code,
      name: bridge.name,
      buildingA: bridge.buildingA,
      buildingB: bridge.buildingB,
      floorA: bridge.floorA,
      floorB: bridge.floorB,
    } as IBridgeDTO;
  }

  public static toDomain (bridge: any | Model<IBridgePersistence & Document> ): Bridge {
    const bridgeOrError = Bridge.create(
      bridge,
      new UniqueEntityID(bridge.domainId)
    );

    bridgeOrError.isFailure ? console.log(bridgeOrError.error) : '';

    return bridgeOrError.isSuccess ? bridgeOrError.getValue() : null;
  }

  public static toPersistence (bridge: Bridge): any {
    return {
      domainId: bridge.id.toString(),
      code: bridge.code,
      name: bridge.name,
      buildingA: bridge.buildingA,
      buildingB: bridge.buildingB,
      floorA: bridge.floorA,
      floorB: bridge.floorB,
    }
  }
}
