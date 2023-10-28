import {Mapper} from "../core/infra/Mapper";
import {Room} from "../domain/room";
import {IRoomDTO} from "../dto/IRoomDTO";
import {IRoomPersistence} from "../dataschema/IRoomPersistence";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";


export class RoomMap extends Mapper<Room> {

  public static toDTO(room:Room): IRoomDTO {
    return {
      id: room.id.toString(),
      buildingId: room.building,
      floorId: room.floor,
      name: room.name,
      roomType: room.type,
      description: room.description,
    } as IRoomDTO;
  }

  public static toDomain(room: any | Model<IRoomPersistence & Document>): Room {
   const roomOrError = Room.create(
     {
       buildingId: room.buildingId,
       floorId: room.floorId,
       name: room.name,
       roomType: room.roomType,
       description: room.description,
     },
     new UniqueEntityID(room.domainId)
   );

   roomOrError.isFailure ? console.log(roomOrError.error) : '';
   return roomOrError.isSuccess ? roomOrError.getValue() : null;
  }

  public static toPersistence(room: Room): any {
    const a = {
      domainId: room.id,
      buildingId: room.building,
      floorId: room.floor,
      name: room.name,
      roomType: room.type,
      description: room.description
    }
    return a;
  }

}
