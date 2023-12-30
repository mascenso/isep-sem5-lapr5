import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";

interface TaskVigilanceProps {
    description: string;
    buildingId: string;
    floors:object[];
    startPosition: number[];
    endPosition: number[];
    contactNumber:number;
    user:object;
    approved:boolean;
    pending:boolean;
    planned:boolean;
}

export class TaskVigilance extends AggregateRoot<TaskVigilanceProps> {

  private constructor (props: TaskVigilanceProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: TaskVigilanceProps, id?: UniqueEntityID): Result<TaskVigilance> {

    const guardedProps = [
      { argument: props.description, argumentName: 'description' },
      { argument: props.buildingId, argumentName: 'buildingId' },
      { argument: props.floors, argumentName: 'floors' },
      { argument: props.startPosition, argumentName: 'startPosition' },
      { argument: props.endPosition, argumentName: 'endPosition' },
      { argument: props.contactNumber, argumentName: 'contactNumber' },
      { argument: props.user, argumentName: 'user' },
      { argument: props.approved, argumentName: 'approved' },
      { argument: props.pending, argumentName: 'pending' },
      { argument: props.planned, argumentName: 'planned' }
    ];


    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<TaskVigilance>(guardResult.message)
    }
    else {
      const task = new TaskVigilance({
        ...props
      }, id);

      return Result.ok<TaskVigilance>(task);
    }
  }

  get id (): UniqueEntityID {
    return this._id;
  }

  public get description() : string {
    return this.props.description;
  }
  public get buildingId() : string {
    return this.props.buildingId;
  }

  public get floors() : object[] {
    return this.props.floors;
  }

  public get contactNumber() : number {
    return this.props.contactNumber;
  }

  public get user() : object {
    return this.props.user;
  }
  public get approved() : boolean {
    return this.props.approved;
  }
  public get pending() : boolean {
    return this.props.pending;
  }
  public get planned() : boolean {
    return this.props.planned;
  }
  public get startPosition() : number[] {
    return this.props.startPosition;
  }
  public get endPosition() : number[] {
    return this.props.endPosition;
  }


  set description ( value: string) {
    this.props.description = value;
  }

  set buildingId ( value: string) {
    this.props.buildingId = value;
  }

  set floors ( value: object[]) {
    this.props.floors = value;
  }

  set contactNumber ( value: number) {
    this.props.contactNumber = value;
  }

  set user ( value: object) {
    this.props.user = value;
  }
  set approved ( value: boolean) {
    this.props.approved = value;
  }
  set pending ( value: boolean) {
    this.props.pending = value;
  }
  set planned ( value: boolean) {
    this.props.planned = value;
  }

  set startPosition ( value: number[]) {
    this.props.startPosition = value;
  }
  set endPosition ( value: number[]) {
    this.props.endPosition = value;
  }

}
