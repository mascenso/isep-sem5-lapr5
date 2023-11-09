import { Result } from "../../core/logic/Result";
import {IBuildingDTO} from "../../dto/IBuildingDTO";
import {BuildingId} from "../../domain/building-agg/buildingId";

export default interface IBuildingService  {
  createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  getAllBuildings(): Promise<Result<IBuildingDTO[]>>;
  getBuildingById(buildingId: BuildingId | string): Promise<Result<IBuildingDTO>>;
}
