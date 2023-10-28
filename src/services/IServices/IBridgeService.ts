import { Result } from "../../core/logic/Result";
import IBridgeDTO from "../../dto/IBridgeDTO";

export default interface IBridgeService  {
  createBridge(bridgeDTO: IBridgeDTO): Promise<Result<IBridgeDTO>>;
  updateBridge(roleDTO: IBridgeDTO, roleId: string): Promise<Result<IBridgeDTO>>;

  getBridge (roleId: string): Promise<Result<IBridgeDTO>>;
  getAllBridges(): Promise<Result<IBridgeDTO[]>>;

  getBridgesBetweenBuildings(building1: string, building2: string): Promise<Result<IBridgeDTO[]>>;
}
