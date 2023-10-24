import { Result } from "../../core/logic/Result";
import IBridgeDTO from "../../dto/IBridgeDTO";

export default interface IBridgeService  {
  createBridge(floorDTO: IBridgeDTO): Promise<Result<IBridgeDTO>>;
  updateBridge(roleDTO: IBridgeDTO): Promise<Result<IBridgeDTO>>;

  getBridge (roleId: string): Promise<Result<IBridgeDTO>>;
}
