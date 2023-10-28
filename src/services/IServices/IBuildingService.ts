import { Result } from "../../core/logic/Result";
import IBuildingBridgeDTO from "../../dto/IBuildingBridgeDTO";
import {IBuildingDTO} from "../../dto/IBuildingDTO";

export default interface IBuildingService  {
  getBuildingBridges(buildingId: string): Promise<Result<IBuildingBridgeDTO[]>>;
  getBuildingBridges(buildingId: string): unknown;
  createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  getAllBuildings(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO[]>>;
}