import { expect } from 'chai';
import { UniqueEntityID } from '../src/core/domain/UniqueEntityID';
import { TaskPickupDelivery } from '../src/domain/task-agg/TaskPickupDelivery';
import { LocationRoom } from '../src/domain/task-agg/locationRoom';


describe('Task PickUp - Unit Test', () => { 
   /*
  it('should create a valid TaskPickUp instance', () => {
  
    const taskProps = {
      description: 'Pickup Task 1',
      pickupLocalization: { buildingId: '12', floor: {}, room: [1, 2]},
      deliveryLocalization: { buildingId: '1212', floor: {}, room: [4, 2]},
      contactNumber: 123456789,
      user: {
        userName:'Zé', contactNumber: 987654321
      },
      deliveryContact: {
        name: 'Zé',
        contactNumber: 987654321
      },
      pickupContact: {
        name: 'Rui',
        contactNumber: 98172322
      },
      approved: true,
      pending: false,
      planned: false
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
    expect(taskPickupDelivery.approved).to.equal(taskProps.approved);
    expect(taskPickupDelivery.pending).to.equal(taskProps.pending);
    expect(taskPickupDelivery.planned).to.equal(taskProps.planned);
  });
  */
});
