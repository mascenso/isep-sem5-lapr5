import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TasksService } from 'src/app/services/tasks.service';
import { of } from 'rxjs';
import { TaskPickupRequestDTO } from 'src/dto/taskPickupDTO';
import { TaskVigilanceRequestDTO } from 'src/dto/taskVigilanceDTO';
import { TaskPlanningComponent } from 'src/app/home/tasks/task-planning/task-planning.component';
import { TaskViewModel } from 'src/app/viewModel/taskView';

describe('TaskPlanningComponent', () => {
    let component: TaskPlanningComponent;
    let tasksService: jasmine.SpyObj<TasksService>;
    let fixture: ComponentFixture<TaskPlanningComponent>;

    beforeEach(() => {
        const tasksServiceSpy = jasmine.createSpyObj('TasksService', ['getAllPickupDeliveryApprovedTasks', 'getAllVigilanceApprovedTasks']);
        // Configura o comportamento dos spy para retornar um Observable quando os métodos forem chamados
        tasksServiceSpy.getAllPickupDeliveryApprovedTasks.and.returnValue(of([]));
        tasksServiceSpy.getAllVigilanceApprovedTasks.and.returnValue(of([]));

        TestBed.configureTestingModule({
            declarations: [TaskPlanningComponent],
            providers: [{ provide: TasksService, useValue: tasksServiceSpy }]
        });

        tasksService = TestBed.inject(TasksService) as jasmine.SpyObj<TasksService>;
        fixture = TestBed.createComponent(TaskPlanningComponent);
        component = fixture.componentInstance;
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should load approved pickup tasks and update approved pickup tasks list', () => {
        const approvedPickupTasksMock: TaskPickupRequestDTO[] = [
            {
                id: '1',
                description: 'Pickup Task 1',
                pickupLocalization: {
                    buildingId: 'ID EDIFICO GRANDE 1',
                    floor: {},
                    room: [1, 2]
                },
                deliveryLocalization: {
                    buildingId: 'ID EDIFICO PEQUENO 1',
                    floor: {},
                    room: [4, 6]
                },
                contactNumber: 123456789,
                user: {},
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
            },
            {
                id: '2',
                description: 'Pickup Task 2',
                pickupLocalization: {
                    buildingId: 'ID EDIFICO GRANDE 2',
                    floor: {},
                    room: [7, 2]
                },
                deliveryLocalization: {
                    buildingId: 'ID EDIFICO PEQUENO 2',
                    floor: {},
                    room: [10, 21]
                },
                contactNumber: 123456789,
                user: {},
                deliveryContact: {
                    name: 'Paulo',
                    contactNumber: 987654322
                },
                pickupContact: {
                    name: 'Ana',
                    contactNumber: 123124121
                },
                approved: true,
                pending: false,
                planned: false
            }
        ];
        tasksService.getAllPickupDeliveryApprovedTasks.and.returnValue(of(approvedPickupTasksMock));

        component.getApprovedPickupTasks();

        expect(tasksService.getAllPickupDeliveryApprovedTasks).toHaveBeenCalled();
        expect(component.approvedPickupTasks.length).toBe(2);
        expect(component.approvedPickupTasks.map(task => task.description)).toEqual(['Pickup Task 1', 'Pickup Task 2']);
    });

    it('should load approved vigilance tasks and update approved vigilance tasks list', () => {

        const approvedVigilanceTasksMock: TaskVigilanceRequestDTO[] = [

            {
                id: '1',
                description: 'Vigilance 1',
                buildingId: 'ID EDIFICO GRANDE 1',
                floors: [{}],
                contactNumber: 123456789,
                user: {},
                approved: true,
                pending: false,
                planned: false
            },
            {
                id: '2',
                description: 'Vigilance 2',
                buildingId: 'ID EDIFICO GRANDE 2',
                floors: [{}],
                contactNumber: 9876433567,
                user: {},
                approved: true,
                pending: false,
                planned: false
            }
        ];

        tasksService.getAllVigilanceApprovedTasks.and.returnValue(of(approvedVigilanceTasksMock));

        component.getApprovedVigilanceTasks();

        expect(tasksService.getAllVigilanceApprovedTasks).toHaveBeenCalled();
        expect(component.approvedVigilanceTasks.length).toBe(2);
        expect(component.approvedVigilanceTasks.map(task => task.description)).toEqual(['Vigilance 1', 'Vigilance 2']);
    });

    it('should map tasks to view model correctly', () => {
        const taskMock = {
            id: '1',
            description: 'Vigilance 1',
            buildingId: 'ID EDIFICO GRANDE 1',
            floors: [{}],
            contactNumber: 123456789,
            user: {},
            approved: false,
            pending: true,
            planned: true,
            type: 'Vigilance'
        };
        const mappedTask = component.mapToTaskViewModel(taskMock, 'Vigilance');

        expect(mappedTask.description).toBe('Vigilance 1');
        expect(mappedTask.type).toBe('Vigilance');
    });

    it('should extract vigilance task details correctly', () => {
        const vigilanceTasksMock = [
            {
                id: '1',
                buildingId: '1',
                description: 'Vigilance Task 1',
                floors: [{ description: 'Floor 1', floorId: '1', floorNumber: 1 }], 
                endPosition: [5, 10],
                startPosition: [2, 3],
                contactNumber: 123456789,
                contact: 712631200,
                approved: true,
                pending: false,
                planned: true,
                user: 'Tiago',
                type: 'Vigilance' as const
            },
            {
                id: '2',
                buildingId: '2',
                description: 'Vigilance Task 2',
                floors: [{ description: 'Floor 2', floorId: '2', floorNumber: 2 }],
                endPosition: [2, 4],
                startPosition: [1, 5],
                contactNumber: 987654321,
                contact: 124135121,
                approved: true,
                pending: false,
                planned: true,
                user: 'Renato',
                type: 'Vigilance' as const
            }
        ];

        const extractedDetails = component.extractVigilanceTaskDetails(vigilanceTasksMock);

        expect(extractedDetails.length).toBe(vigilanceTasksMock.length);
        //Verifica se os detalhes estão sendo extraídos corretamente para cada tarefa de vigilância
        //return [description, startPositionArrayX, startPositionArrayY, floorsDescription, endPositionArrayX, endPositionArrayY, floorsDescription];

        expect(extractedDetails[0][0]).toBe('Vigilance Task 1');
        expect(extractedDetails[0][1]).toBe(2);
        expect(extractedDetails[0][2]).toBe(3);
        expect(extractedDetails[0][4]).toBe(5);
        expect(extractedDetails[0][5]).toBe(10);

        expect(extractedDetails[1][0]).toBe('Vigilance Task 2');
        expect(extractedDetails[1][1]).toBe(1);
        expect(extractedDetails[1][2]).toBe(5);
        expect(extractedDetails[1][4]).toBe(2);
        expect(extractedDetails[1][5]).toBe(4);

    });


    it('should extract pickup task details correctly', () => {
        const pickupTasksMock = [
            {
                id: '1',
                description: 'Pickup Task 1',
                pickupLocalization: { buildingId: 'ID 1', floor: { code: 'A', floorNumber: '1' }, room: [2, 3] },
                deliveryLocalization: { buildingId: 'ID 2', floor: { code: 'B', floorNumber: '2' }, room: [5, 6] },
                contactNumber: 123456789,
                deliveryContact: { name: 'Delivery Name', contactNumber: 987654321 },
                pickupContact: { name: 'Pickup Name', contactNumber: 987654322 },
                approved: true,
                pending: false,
                planned: true,
                contact: 124135121,
                user: 'Renato',
                type: 'Pickup' as const
            }
        ];

        const extractedDetails = component.extractPickupTaskDetails(pickupTasksMock);

        expect(extractedDetails.length).toBe(pickupTasksMock.length);
        // Verifica se os detalhes estão sendo extraídos corretamente para cada tarefa de pick up
        //return [description, pickupRoomX, pickupRoomY, pickupFloorCode, deliveryroomX, deliveryroomY, deliveryFloorCode];
        expect(extractedDetails[0][0]).toBe('Pickup Task 1');
        expect(extractedDetails[0][1]).toBe(2);
        expect(extractedDetails[0][2]).toBe(3);
        expect(extractedDetails[0][3]).toBe('A');
        expect(extractedDetails[0][4]).toBe(5);
        expect(extractedDetails[0][5]).toBe(6);
        expect(extractedDetails[0][6]).toBe('B');
    });
});