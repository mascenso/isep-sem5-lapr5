import {ValueObject} from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface TaskStatusProps {
  approved:boolean;
  pending:boolean;
  planned:boolean;
}

export class TaskStatusVO extends ValueObject<TaskStatusProps>{

  private constructor(props: TaskStatusProps) {
    super(props);
  }

  public static create(approved:boolean, pending:boolean, planned:boolean): Result<TaskStatusVO> {

      return  Result.ok<TaskStatusVO>(new TaskStatusVO({approved, pending, planned}));
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

  public set approved(value : boolean) {
    this.props.approved = value;
  }

  public set pending(value : boolean) {
    this.props.pending = value;
  }

  public set planned(value : boolean) {
    this.props.planned = value;
  }
}


