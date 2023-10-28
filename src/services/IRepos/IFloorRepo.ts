import { Repo } from "../../core/infra/Repo";
import { BuildingId } from "../../domain/buildingId";
import {Floor} from "../../domain/floor";
import {FloorId} from "../../domain/floorId";

export default interface IFloorRepo extends Repo<Floor> {
  save(floor: Floor): Promise<Floor>;
  findByDomainId (floorId: FloorId | string): Promise<Floor>;
  getFloorsAtBuildings(building: BuildingId | string): Promise<Floor[]>;
  getBuildingsByMinMaxFloors (minFloors: number, maxFloors: number): Promise<BuildingId[]>;
  getAllFloors(): Promise<Floor[]>;
}
