import { Repo } from "../../core/infra/Repo";
import {Elevator} from "../../domain/elevator";
import {ElevatorId} from "../../domain/elevatorId";

export default interface IElevatorRepo extends Repo<Elevator> {
  save(elevator: Elevator): Promise<Elevator>;
 // findByDomainId (elevatorId: ElevatorId | string): Promise<Elevator>;
 getAllElevators (): Promise<Elevator[]>;
}
