import { expect } from 'chai';
import { UniqueEntityID } from '../src/core/domain/UniqueEntityID';
import { TaskVigilance } from '../src/domain/task-agg/TaskVigilance';
import { TaskStatusVO } from "../src/domain/task-agg/taskStatusVO";
import { TaskStatus } from "../src/domain/task-agg/TaskStatus";

describe('TaskVigilance - Unit Test', () => {
  it('should create a valid TaskVigilance instance', () => {
    const taskProps = {
      description: 'Test Task',
      buildingId: '12314',
      floors: [{ floorNumber: 1 }, { floorNumber: 2 }],
      startPosition: [1, 2],
      endPosition: [4, 10],
      contactNumber: 123456789,
      user: { name: 'Zé' },
      taskStatus: TaskStatusVO.create( false, true, false ).getValue()
    };

    // Cria a tarefa de vigilância
    const taskResult = TaskVigilance.create(taskProps, new UniqueEntityID());

    // Verifica se a criação foi bem-sucedida
    expect(taskResult.isSuccess).to.be.true;

    // Obtem a instância da tarefa de vigilância
    const taskVigilance = taskResult.getValue();

    // Valida as propriedades da tarefa
    expect(taskVigilance.description).to.equal(taskProps.description);
    expect(taskVigilance.buildingId).to.equal(taskProps.buildingId);
    expect(taskVigilance.floors).to.deep.equal(taskProps.floors);
    expect(taskVigilance.startPosition).to.deep.equal(taskProps.startPosition);
    expect(taskVigilance.endPosition).to.deep.equal(taskProps.endPosition);
    expect(taskVigilance.contactNumber).to.equal(taskProps.contactNumber);
    expect(taskVigilance.user).to.deep.equal(taskProps.user);
    expect(taskVigilance.taskStatus.approved).to.equal(taskProps.taskStatus.approved);
    expect(taskVigilance.taskStatus.pending).to.equal(taskProps.taskStatus.pending);
    expect(taskVigilance.taskStatus.planned).to.equal(taskProps.taskStatus.planned);
  });


  it('should fail to create a TaskVigilance with missing required properties', () => {
    const taskProps = {
      description: 'Incomplete Task',

      buildingId: 'um-id-muito-grande',
      floors: null,
      startPosition: [3, 2],
      contactNumber: 123456789,
      endPosition: [6, 4],
      user: { name: 'Joana' },
      taskStatus: TaskStatusVO.create( true, false, false ).getValue()

    };

    // Cria a tarefa de vigilância com propriedades em falta
    const taskResult = TaskVigilance.create(taskProps, new UniqueEntityID());

    // Verifica se a criação falhou (deve falhar por causa das propriedades em falta)
    expect(taskResult.isFailure).to.be.true;
  });

  describe('TaskVigilance', () => {
    const validTaskProps = {
      description: 'Test Task',
      buildingId: '12314',
      floors: [{ floorNumber: 1 }, { floorNumber: 2 }],
      startPosition: [1, 2],
      endPosition: [4, 10],
      contactNumber: 123456789,
      user: { name: 'Zé' },
      taskStatus: TaskStatusVO.create( false, true, false ).getValue()
    };

    it('should update task status to APPROVED', () => {
      const task = TaskVigilance.create(validTaskProps).getValue();
      task.updateTaskStatus(TaskStatus.APPROVED);

      expect(task.taskStatus.approved).to.be.true;
      expect(task.taskStatus.pending).to.be.false;
      expect(task.taskStatus.planned).to.be.false;
    });

    it('should update task status to PENDING', () => {
      const task = TaskVigilance.create(validTaskProps).getValue();
      task.updateTaskStatus(TaskStatus.PENDING);

      expect(task.taskStatus.approved).to.be.false;
      expect(task.taskStatus.pending).to.be.true;
      expect(task.taskStatus.planned).to.be.false;
    });

    it('should update task status to PLANNED', () => {
      const task = TaskVigilance.create(validTaskProps).getValue();
      task.updateTaskStatus(TaskStatus.PLANNED);

      expect(task.taskStatus.approved).to.be.true;
      expect(task.taskStatus.pending).to.be.false;
      expect(task.taskStatus.planned).to.be.true;
    });

    it('should update task status to REJECTED', () => {
      const task = TaskVigilance.create(validTaskProps).getValue();
      task.updateTaskStatus(TaskStatus.REJECTED);

      expect(task.taskStatus.approved).to.be.false;
      expect(task.taskStatus.pending).to.be.false;
      expect(task.taskStatus.planned).to.be.false;
    });

    it('should throw an error for an invalid task status', () => {
      const task = TaskVigilance.create(validTaskProps).getValue();

      expect(() => task.updateTaskStatus('INVALID_STATUS' as TaskStatus)).to.throw('TaskStatus not valid');
    });
  });





});
