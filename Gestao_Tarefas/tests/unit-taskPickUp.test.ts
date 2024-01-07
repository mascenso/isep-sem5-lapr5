import { expect } from 'chai';
import { UniqueEntityID } from '../src/core/domain/UniqueEntityID';
import { TaskPickupDelivery } from '../src/domain/task-agg/TaskPickupDelivery';
import { LocationRoom } from '../src/domain/task-agg/locationRoom';
import { TaskStatusVO } from "../src/domain/task-agg/taskStatusVO";
import { User } from "../src/domain/task-agg/user";


describe('Task PickUp - Unit Test', () => {

  it('should create a valid TaskPickUp instance', () => {

    const taskProps = {
      description: 'Pickup Task 1',
      pickupLocalization: LocationRoom.create('12', {"floorId":"id-muito-grande","floorNumber":3, "description":"a1"}, [1, 2]).getValue(),
      deliveryLocalization: LocationRoom.create('1212', {"floorId":"id-muito-grande","floorNumber":3, "description":"a1"}, [4, 2]).getValue(),
      contactNumber: 123456789,
      user: User.create('Zé', 987654321, '1210888@isep.ipp.pt').getValue(),
      deliveryContact: User.create('Zé', 987654321, '1210888@isep.ipp.pt').getValue(),
      pickupContact: User.create('Rui', 981722222, '1111111@isep.ipp.pt').getValue(),
      taskStatus: TaskStatusVO.create( true, false, false ).getValue()
    };

    // Cria a tarefa de pickup delivery
    const taskResult = TaskPickupDelivery.create(taskProps, new UniqueEntityID());

    // Verifica se a criação foi bem-sucedida
    expect(taskResult.isSuccess).to.be.true;

    // Obtem a instância da tarefa de pickup delivery
    const taskPickupDelivery = taskResult.getValue();

    // Valida as propriedades da tarefa
    expect(taskPickupDelivery.description).to.equal(taskProps.description);
    expect(taskPickupDelivery.pickupLocalization).to.deep.equal(taskProps.pickupLocalization);
    expect(taskPickupDelivery.deliveryLocalization).to.deep.equal(taskProps.deliveryLocalization);
    expect(taskPickupDelivery.contactNumber).to.equal(taskProps.contactNumber);
    expect(taskPickupDelivery.user).to.deep.equal(taskProps.user);
    expect(taskPickupDelivery.deliveryContact).to.deep.equal(taskProps.deliveryContact);
    expect(taskPickupDelivery.pickupContact).to.deep.equal(taskProps.pickupContact);
    expect(taskPickupDelivery.taskStatus.approved).to.equal(taskProps.taskStatus.approved);
    expect(taskPickupDelivery.taskStatus.pending).to.equal(taskProps.taskStatus.pending);
    expect(taskPickupDelivery.taskStatus.planned).to.equal(taskProps.taskStatus.planned);
  });

});
