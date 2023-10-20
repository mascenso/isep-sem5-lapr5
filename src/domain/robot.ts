import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import { Result } from "../core/logic/Result";
import { RobotId } from "./robotId";

import IRobotDTO from "../dto/IRobotDTO";

interface RobotProps {
  designacao: string;
  tarefas: string;
}

export class Robot extends AggregateRoot<RobotProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get robotId (): RobotId {
    return new RobotId(this.robotId.toValue());
  }

  get designacao (): string {
    return this.props.designacao;
  }

  get tarefas (): string {
    return this.props.tarefas;
  }
  
  set designacao ( value: string) {
    this.props.designacao = value;
  }

  set tarefas ( value: string) {
    this.props.tarefas = value;
  }

  private constructor (props: RobotProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (robotDTO: IRobotDTO, id?: UniqueEntityID): Result<Robot> {
    const designacao = robotDTO.designacao;
    const tarefas = robotDTO.tarefas;

    if (!!designacao === false || designacao.length === 0) {
      return Result.fail<Robot>('Must provide a robot designation.')
    } else {
      const robot = new Robot({ designacao: designacao,
      tarefas: tarefas }, id);
      return Result.ok<Robot>( robot )
    }
  }
}
