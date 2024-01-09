import {ValueObject} from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface LocationRoomProps {
  buildingId:String;
  floor:object;
  room: number[];
}

export class LocationRoom extends ValueObject<LocationRoomProps>{

  private constructor(props: LocationRoomProps) {
    super(props);
  }

  public static create(buildingId:String, floor:object, room: number[]): Result<LocationRoom> {

    if (buildingId ==null || floor==null || room==null ) {
      return Result.fail<LocationRoom>('Need buildingId, floor and room');
    }

    return  Result.ok<LocationRoom>(new LocationRoom({buildingId, floor, room}));
  }


}


