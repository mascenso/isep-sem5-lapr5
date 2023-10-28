import { Repo } from "../../core/infra/Repo";
import {Bridge} from "../../domain/bridge";
import {BridgeId} from "../../domain/bridgeId";

export default interface IBridgeRepo extends Repo<Bridge> {

  save(bridge: Bridge, buildingAId?:string, buildingBId?:string): Promise<Bridge>;

  findByDomainId (bridgeId: BridgeId | string): Promise<Bridge>;

  getAllBridges (): Promise<Bridge[]>;
  getBridgesForBuilding( buildingId: string): Promise<any>;
  getBridgesBetweenBuildings(building1: string, building2: string): Promise<Bridge[]>;

  areConnected(floorAId: string, floorBId: string): Promise<Boolean>;
}
