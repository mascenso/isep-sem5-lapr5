import { Repo } from "../../core/infra/Repo";
import {Building} from "../../domain/building-agg/building";
import {BuildingId} from "../../domain/building-agg/buildingId";

export default interface IBuildingRepo extends Repo<Building> {
  save(building: Building): Promise<Building>;
  findByDomainId (buildingId: BuildingId | string): Promise<Building>;
  findByDomainIds(buildingIds: BuildingId[] | string): Promise<Building[]>;
  getAllBuildings (): Promise<Building[]>;

}
