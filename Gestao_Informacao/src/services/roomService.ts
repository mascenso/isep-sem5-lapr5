import IRoomService from "./IServices/IRoomService";
import {Inject, Service} from "typedi";
import config from "../../config";
import IRoomRepo from "./IRepos/IRoomRepo";
import {Result} from "../core/logic/Result";
import {IRoomDTO} from "../dto/IRoomDTO";
import {Room} from "../domain/room-agg/room";
import {RoomMap} from "../mappers/RoomMap";
import IFloorRepo from "./IRepos/IFloorRepo";
import IBuildingRepo from "./IRepos/IBuildingRepo";
import {RoomType} from "../domain/room-agg/roomType";

@Service()
export default class RoomService implements IRoomService {

  constructor(
    @Inject(config.repos.room.name) private roomRepo : IRoomRepo,
    @Inject(config.repos.floor.name) private floorRepo : IFloorRepo,
    @Inject(config.repos.building.name) private buildingRepo : IBuildingRepo
) {}

  public async createRoom(roomDTO: IRoomDTO): Promise<Result<IRoomDTO>> {
    try {

      const floor = await this.floorRepo.findByDomainId(roomDTO.floorId);
      if (floor == null) {
        return Result.fail<IRoomDTO>(`Floor with id ${roomDTO.floorId} not found!`);
      }

      const building = await this.buildingRepo.findByDomainId(roomDTO.buildingId);
      if (building == null) {
        return Result.fail<IRoomDTO>(`Building with id ${roomDTO.buildingId} not found!`);
      }

      if (roomDTO.buildingId !== floor.buildingId) {
        return Result.fail<IRoomDTO>(`Floor with id ${roomDTO.floorId} doesn't belong to building with id ${roomDTO.buildingId}!`);
      }

      const roomOrError = Room.create({
          buildingId:roomDTO.buildingId,
          floorId:roomDTO.floorId,
          name:roomDTO.name,
          description:roomDTO.description,
          roomType: this.parseRoomType(roomDTO.roomType.toLowerCase())
      });

      if (roomOrError.isFailure) {
        return Result.fail<IRoomDTO>(roomOrError.errorValue());
      }

      const roomResult = roomOrError.getValue();

      await this.roomRepo.save(roomResult);

      const roomDTOResult = RoomMap.toDTO( roomResult ) as IRoomDTO;
      return Result.ok<IRoomDTO>( roomDTOResult )
    } catch (e) {
      throw e;
    }
  }

  public async getRoomById(roomId: string): Promise<Result<IRoomDTO>> {
    try {
      const room = await this.roomRepo.findByDomainId(roomId);

      if (room === null) {
        return Result.fail<IRoomDTO>(`Room with id ${roomId} not found!`);
      }
      const roomDTO = RoomMap.toDTO(room) as IRoomDTO;
      return Result.ok<IRoomDTO>(roomDTO);

    } catch (e) {
      throw e;
    }
  }

  private parseRoomType(roomTypeStr: string): RoomType | undefined {
    const roomType = Object.values(RoomType).find((value) => value === roomTypeStr);
    return roomType as RoomType | undefined;
  }

}
