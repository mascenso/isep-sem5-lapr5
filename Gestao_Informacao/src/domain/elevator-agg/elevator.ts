import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";

interface ElevatorProps {
  code: string;
  buildingId: string;
  floorList: string[];
}

export class Elevator extends AggregateRoot<ElevatorProps> {

  private constructor (props: ElevatorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: ElevatorProps, id?: UniqueEntityID): Result<Elevator> {

    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.floorList, argumentName: 'floorList' },
      { argument: props.buildingId, argumentName: 'buldingId' }
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Elevator>(guardResult.message)
    }
    else {
      const elevator = new Elevator({
        ...props
      }, id);

      return Result.ok<Elevator>(elevator);
    }
  }

  get id (): UniqueEntityID {
    return this._id;
  }

  get floorList() : string[] {
    return this.props.floorList;
  }

  get code() : string {
    return this.props.code;
  }

  get buildingId() : string {
    return this.props.buildingId;
  }

  set code ( value: string) {
    this.props.code = value;
  }

  set floorList (value: string[]) {
    this.props.floorList=value;
  }


}
