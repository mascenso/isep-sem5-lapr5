import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import IRobotTypeDTO from "../../dto/IRobotTypeDTO";
import TaskType from '../../enums/taskType';
import {Guard} from "../../core/logic/Guard";

interface RobotTypeProps {
  designacao: string;
  tipoTarefas: TaskType[];
}


/**
 * Tipo B: Robot marca W modelo Z com capacidade de executar tarefas de vigilância e pickeup&delivery
 */
/*
tipo de robot: obrigatório, alfanum+ericos, maximo 25 caracteres
marca: obrigatório, maximo 50 caracteres
modelo: obrigatório, máximo 100 caracteres
*/
"o \"tipo de robot\" é um código identificativo desse tipo de robots."

export class RobotType extends AggregateRoot<RobotTypeProps> {

  private constructor (props: RobotTypeProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: RobotTypeProps, id?: UniqueEntityID): Result<RobotType> {
    const guardedProps = [
      { argument: props.designacao, argumentName: 'designacao' },
      { argument: props.tipoTarefas, argumentName: 'tipoTarefas' }
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<RobotType>(guardResult.message)
    }
    else {
      const robotType = new RobotType({
        ...props
      }, id);

      return Result.ok<RobotType>( robotType );
    }
  }

  get id (): UniqueEntityID {
    return this._id;
  }

  get designacao (): string {
    return this.props.designacao;
  }

  get tipoTarefas (): TaskType[] {
    return this.props.tipoTarefas;
  }

  set designacao ( value: string) {
    this.props.designacao = value;
  }

  set tipoTarefas ( value: TaskType[]) {
    this.props.tipoTarefas = value;
  }

}
