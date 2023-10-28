import { Result } from "../../core/logic/Result";
import IBridgeDTO from "../../dto/IBridgeDTO";
import IBuildingBridgeDTO from "../../dto/IBuildingBridgeDTO";

export default interface IBridgeService  {
  createBridge(bridgeDTO: IBridgeDTO): Promise<Result<IBridgeDTO>>;
  updateBridge(roleDTO: IBridgeDTO): Promise<Result<IBridgeDTO>>;

  getBridge (roleId: string): Promise<Result<IBridgeDTO>>;
  getAllBridges(): Promise<Result<IBridgeDTO[]>>;

  getBridgesAtBuildings(building1: string, building2: string): Promise<Result<IBridgeDTO[]>>;

  getBuildingBridges(buildingId: string): Promise<Result<IBuildingBridgeDTO[]>>;
  getBuildingBridges(buildingId: string): unknown;
}
