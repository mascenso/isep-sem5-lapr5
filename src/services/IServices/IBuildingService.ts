import { Result } from "../../core/logic/Result";
import {IBuildingDTO} from "../../dto/IBuildingDTO";

export default interface IBuildingService  {
  createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;
  getAllBuildings(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO[]>>;
  getBuildingsByMinMaxFloors(minFloors: number, maxFloors: number): Promise<Result<IBuildingDTO[]>>;
}
