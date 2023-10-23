import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Result} from "../core/logic/Result";
import {Guard} from "../core/logic/Guard";

interface ElevatorProps {
  code: string;
  coordX: number; 
  coordY: number;
}

export class Elevator extends AggregateRoot<ElevatorProps> {

  private constructor (props: ElevatorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: ElevatorProps, id?: UniqueEntityID): Result<Elevator> {

    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.coordX, argumentName: 'coordX' },
      { argument: props.coordY, argumentName: 'coordY' }
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

  get code() : string {
    return this.props.code;
  }

  get coordX() : number {
    return this.props.coordX;
  }

  get coordY() : number {
    return this.props.coordY;
  }

  set code ( value: string) {
    this.props.code = value;
  }

  set coordX ( value: number) {
    this.props.coordX = value;
  }

  set coordY ( value: number) {
    this.props.coordY = value;
  }

}
