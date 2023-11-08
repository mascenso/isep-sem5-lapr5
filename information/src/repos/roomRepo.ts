import {Inject, Service} from "typedi";
import IRoomRepo from "../services/IRepos/IRoomRepo";
import { Room } from "../domain/room-agg/room";
import {RoomId} from "../domain/room-agg/roomId";
import {Document, FilterQuery, Model} from "mongoose";
import {IRoomPersistence} from "../dataschema/IRoomPersistence";
import {RoomMap} from "../mappers/RoomMap";


@Service()
export default class RoomRepo implements IRoomRepo {


  constructor(
    @Inject('roomSchema') private roomSchema: Model<IRoomPersistence & Document>
  ) {}

  public async save(room: Room): Promise<Room> {
    const query = { domainId: room.id.toString()};

    const roomDocument = await this.roomSchema.findOne( query );

    try {
      if (roomDocument === null ) {
        const rawRoom: any = RoomMap.toPersistence(room);

        const roomCreated = await this.roomSchema.create(rawRoom);

        return RoomMap.toDomain(roomCreated);
      } else {
        roomDocument.name = room.name;
        await roomDocument.save();

        return room;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId(roomId: RoomId | string): Promise<Room> {
    const query = { domainId: roomId};

    const roomRecord = await this.roomSchema.findOne( query as FilterQuery<IRoomPersistence & Document> );

    if( roomRecord != null) {
      return RoomMap.toDomain(roomRecord);
    }
    else {
      return null;
    }
  }

  public async exists(room: Room): Promise<boolean> {
    const roomId = room.id instanceof RoomId ? (<RoomId>room.id).toValue() : room.id;
    const query = { domainId: roomId};
    const roomDocument = await this.roomSchema.findOne( query as FilterQuery<IRoomPersistence & Document>);
    return !!roomDocument === true;
  }

}
