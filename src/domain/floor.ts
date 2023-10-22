import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Result} from "../core/logic/Result";
import {Guard} from "../core/logic/Guard";

interface FloorProps {
    buildingId: string;
    width: number; 
    length: number;
    floorNumber: number;
    description?: string;
    floorMap: number[][];
}

export class Floor extends AggregateRoot<FloorProps> {

  private constructor (props: FloorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: FloorProps, id?: UniqueEntityID): Result<Floor> {

    const guardedProps = [
      { argument: props.buildingId, argumentName: 'buildingId' },
      { argument: props.width, argumentName: 'width' },
      { argument: props.length, argumentName: 'length' },
      { argument: props.floorNumber, argumentName: 'floorNumber' },
      { argument: props.description, argumentName: 'description' },
      { argument: props.floorMap, argumentName: 'floorMap' }
    ];


    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Floor>(guardResult.message)
    }
    else {
      const floor = new Floor({
        ...props
      }, id);

      return Result.ok<Floor>(floor);
    }
  }

  addMap(floorMap){
    this.floorMap = floorMap;
    return this;
  }

  get id (): UniqueEntityID {
    return this._id;
  }

  get buildingId() : string {
    return this.props.buildingId;
  }

  get floorNumber() : number {
    return this.props.floorNumber;
  }


  get description() : string {
    return this.props.description;
  }

  get width(): number {
    return this.props.width;
  }

  get length(): number {
    return this.props.length;
  }

  get floorMap(): number [][]{
    return this.props.floorMap;
  }

  set buildingId ( value: string) {
    this.props.buildingId = value;
  }

  set description ( value: string) {
    this.props.description = value;
  }

  set floorNumber ( value: number) {
    this.props.floorNumber = value;
  }

  set width ( value: number) {
    this.props.width = value;
  }

  set length ( value: number) {
    this.props.length = value;
  }

  set floorMap ( value: number[][]) {
    this.props.floorMap = value;
  }
}
