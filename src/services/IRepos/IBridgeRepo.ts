import { Repo } from "../../core/infra/Repo";
import {Bridge} from "../../domain/bridge";
import {BridgeId} from "../../domain/bridgeId";

export default interface IBridgeRepo extends Repo<Bridge> {
  save(bridge: Bridge): Promise<Bridge>;
  findByDomainId (bridgeId: BridgeId | string): Promise<Bridge>;
}
