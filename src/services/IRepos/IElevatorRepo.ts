import { Repo } from "../../core/infra/Repo";
import {Elevator} from "../../domain/elevator-agg/elevator";
import {ElevatorId} from "../../domain/elevator-agg/elevatorId";
import {FloorId} from "../../domain/floor-agg/floorId";

export default interface IElevatorRepo extends Repo<Elevator> {
  save(elevator: Elevator): Promise<Elevator>;
  findByDomainId (elevatorId: ElevatorId | string): Promise<Elevator>;
  getAllElevators (): Promise<Elevator[]>;
  findByFloorIds(floorIds: FloorId[] | string[]): Promise<Elevator[]>;
}
