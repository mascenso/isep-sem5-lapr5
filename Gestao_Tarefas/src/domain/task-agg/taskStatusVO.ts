import {ValueObject} from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";
import { TaskStatus } from "./TaskStatus";

interface TaskStatusProps {
  approved:boolean;
  pending:boolean;
  planned:boolean;
  status?:TaskStatus;
}

export class TaskStatusVO extends ValueObject<TaskStatusProps> {

  private constructor(props: TaskStatusProps) {
    props.status = TaskStatusVO.whichStatus(props.approved, props.pending, props.planned);
    super(props);
  }

  public static create(approved: boolean, pending: boolean, planned: boolean): Result<TaskStatusVO> {

    return Result.ok<TaskStatusVO>(new TaskStatusVO({ approved, pending, planned }));
  }

  public get approved(): boolean {
    return this.props.approved;
  }

  public get pending(): boolean {
    return this.props.pending;
  }

  public get planned(): boolean {
    return this.props.planned;
  }

  public set approved(value: boolean) {
    this.props.approved = value;
    this.setTaskStatus();
  }

  public set pending(value: boolean) {
    this.props.pending = value;
    this.setTaskStatus();
  }

  public set planned(value: boolean) {
    this.props.planned = value;
    this.setTaskStatus();
  }

  public get status(): TaskStatus {
    return this.props.status;
  }

  public set status(value: TaskStatus) {
    this.props.status = value;
  }

  public static whichStatus(approved: boolean, pending: boolean, planned: boolean): TaskStatus {
    let status: TaskStatus;

    if (approved) {
      if (planned)
        status = TaskStatus.PLANNED;
      else
        status = TaskStatus.APPROVED;
    } else if (pending)
      status = TaskStatus.PENDING;
    else
      status = TaskStatus.REJECTED;

    return status;
  }

  public setTaskStatus(): void {
    this.props.status = TaskStatusVO.whichStatus(this.approved, this.pending, this.planned);
  }

}


