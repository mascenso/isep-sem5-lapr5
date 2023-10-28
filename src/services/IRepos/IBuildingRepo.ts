import { Repo } from "../../core/infra/Repo";
import {Building} from "../../domain/building";
import {BuildingId} from "../../domain/buildingId";

export default interface IBuildingRepo extends Repo<Building> {
  save(building: Building): Promise<Building>;
  findByDomainId (buildingId: BuildingId | string): Promise<Building>;
  findByDomainIds(buildingIds: BuildingId[] | string): Promise<Building[]>;
  getAllBuildings (): Promise<Building[]>;

}
