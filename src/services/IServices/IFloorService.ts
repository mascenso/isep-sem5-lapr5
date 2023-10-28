import { Result } from "../../core/logic/Result";
import {IFloorDTO} from "../../dto/IFloorDTO";
import {IBuildingDTO} from "../../dto/IBuildingDTO";

export default interface IFloorService  {
  createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
  addMapToFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
  updateFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
  getFloorsAtBuildings(building: string): Promise<Result<IFloorDTO[]>>;
  getBuildingsByMinMaxFloors(minFloors: number, maxFloors: number): Promise<Result<IBuildingDTO[]>>;
  getAllFloors(): Promise<Result<IFloorDTO[]>>;
}
