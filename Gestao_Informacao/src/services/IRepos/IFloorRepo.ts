import { Repo } from "../../core/infra/Repo";
import { BuildingId } from "../../domain/building-agg/buildingId";
import {Floor} from "../../domain/floor-agg/floor";
import {FloorId} from "../../domain/floor-agg/floorId";

export default interface IFloorRepo extends Repo<Floor> {
  save(floor: Floor): Promise<Floor>;
  findByDomainId (floorId: FloorId | string): Promise<Floor>;
  getFloorsAtBuildings(building: BuildingId | string): Promise<Floor[]>;
  getBuildingsByMinMaxFloors (minFloors: number, maxFloors: number): Promise<BuildingId[]>;
  getAllFloors(): Promise<Floor[]>;
  findFloorsByListOfIds(floorIdList: FloorId[] | string[]): Promise<Floor[]>;
}
