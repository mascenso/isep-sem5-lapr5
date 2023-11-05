import {Repo} from "../../core/infra/Repo";
import {Room} from "../../domain/room-agg/room";
import {RoomId} from "../../domain/room-agg/roomId";

export default interface IRoomRepo extends Repo<Room> {
  save(room: Room): Promise<Room>;
  findByDomainId (roomId: RoomId | string): Promise<Room>;
}
