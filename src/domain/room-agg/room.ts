import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {BuildingId} from "../building-agg/buildingId";
import {FloorId} from "../floor-agg/floorId";
import {RoomType} from "./roomType";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";
import {RoomId} from "./roomId";

interface RoomProps {
  buildingId: string;
  floorId: string;
  name: string;
  roomType: RoomType;
  description?: string; //optional
}

export class Room extends AggregateRoot<RoomProps> {

  private constructor (props: RoomProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props:RoomProps, id?: UniqueEntityID): Result<Room> {
    const guardedMandatoryProps = [
      { argument: props.buildingId, argumentName: 'buildingId'},
      { argument: props.floorId, argumentName: 'floorId'},
      { argument: props.name, argumentName: 'name' },
      { argument: props.roomType, argumentName: 'roomType' }
    ];

    const guardMandatoryPropsResult = Guard.againstNullOrUndefinedBulk(guardedMandatoryProps);

    if (!guardMandatoryPropsResult.succeeded) {
      return Result.fail<Room>(guardMandatoryPropsResult.message)
    }
    else {
      const room = new Room({
        buildingId: props.buildingId,
        floorId: props.floorId,
        name: props.name,
        description: props.description,
        roomType: props.roomType
      }, id);

      return Result.ok<Room>(room);
    }

  }

  get id (): UniqueEntityID {
    return this._id;
  }

  get domainId(): string {
    return this.name;
  }

  get name() : string {
    return this.props.name.toString();
  }

  get type() : string {
    return this.props.roomType.toString();
  }

  get building() : string {
    return this.props.buildingId.toString();
  }

  get floor() : string  {
    return this.props.floorId.toString();
  }

  get description() : string {
    return this.props.description;
  }

}
