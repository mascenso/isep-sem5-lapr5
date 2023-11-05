import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";

interface ElevatorProps {
  code: string;
  floorId: string;
  coordX1: number;
  coordY1: number;
  coordX2: number;
  coordY2: number;
}

export class Elevator extends AggregateRoot<ElevatorProps> {

  private constructor (props: ElevatorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: ElevatorProps, id?: UniqueEntityID): Result<Elevator> {

    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.floorId, argumentName: 'floorId' },
      { argument: props.coordX1, argumentName: 'coordX1' },
      { argument: props.coordY1, argumentName: 'coordY1' },
      { argument: props.coordX2, argumentName: 'coordX2' },
      { argument: props.coordY2, argumentName: 'coordY2' }
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

  get floorId() : string {
    return this.props.floorId;
  }

  get code() : string {
    return this.props.code;
  }

  get coordX1() : number {
    return this.props.coordX1;
  }

  get coordY1() : number {
    return this.props.coordY1;
  }

  get coordX2() : number {
    return this.props.coordX2;
  }

  get coordY2() : number {
    return this.props.coordY2;
  }

  set code ( value: string) {
    this.props.code = value;
  }

  set coordX1 ( value: number) {
    this.props.coordX1 = value;
  }

  set coordY1 ( value: number) {
    this.props.coordY1 = value;
  }

  set coordX2 ( value: number) {
    this.props.coordX2 = value;
  }

  set coordY2 ( value: number) {
    this.props.coordY2 = value;
  }

}
