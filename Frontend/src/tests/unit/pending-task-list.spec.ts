import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TasksService } from 'src/app/services/tasks.service';
import { of } from 'rxjs';
import { PendingTaskListComponent } from 'src/app/home/tasks/pending-task-list/pending-task-list.component';
import { TaskPickupRequestDTO } from 'src/dto/taskPickupDTO';
import { TaskVigilanceRequestDTO } from 'src/dto/taskVigilanceDTO';
import { TaskViewModel } from 'src/app/viewModel/taskView';

describe('PendingTaskListComponent', () => {
    let component: PendingTaskListComponent;
    let tasksService: jasmine.SpyObj<TasksService>;

    beforeEach(() => {
        const tasksServiceSpy = jasmine.createSpyObj('TasksService', ['getAllPickupDeliveryPendingTasks', 'getAllVigilancePendingTasks']);

        TestBed.configureTestingModule({
            declarations: [PendingTaskListComponent],
            providers: [{ provide: TasksService, useValue: tasksServiceSpy }]
        });

        tasksService = TestBed.inject(TasksService) as jasmine.SpyObj<TasksService>;
        component = TestBed.createComponent(PendingTaskListComponent).componentInstance;
    });


    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should load pickup tasks and update pending task list', () => {

        const pickupTasksMock: TaskPickupRequestDTO[] = [
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
                approved: false,
                pending: true,
                planned: true
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
                approved: false,
                pending: true,
                planned: true
            }
        ];

        tasksService.getAllPickupDeliveryPendingTasks.and.returnValue(of(pickupTasksMock));

        component.getPickupTasks();

        expect(tasksService.getAllPickupDeliveryPendingTasks).toHaveBeenCalled();
        expect(component.pendingTaskList.length).toBe(2);
        expect(component.pendingTaskList.map(task => task.description)).toEqual(['Pickup Task 1', 'Pickup Task 2']);
    });



    it('should load vigilance tasks and update pending task list', () => {

        const vigilanceTasksMock: TaskVigilanceRequestDTO[] = [

            {
                id: '1',
                description: 'Vigilance 1',
                buildingId: 'ID EDIFICO GRANDE 1',
                floors: [{}],
                contactNumber: 123456789,
                user: {},
                approved: false,
                pending: true,
                planned: true
            },
            {
                id: '2',
                description: 'Vigilance 2',
                buildingId: 'ID EDIFICO GRANDE 2',
                floors: [{}],
                contactNumber: 9876433567,
                user: {},
                approved: false,
                pending: true,
                planned: true
            },
            {
                id: '3',
                description: 'Vigilance 3',
                buildingId: 'ID EDIFICO PEQUENO 3',
                floors: [{}],
                contactNumber: 123124141,
                user: {},
                approved: false,
                pending: true,
                planned: true
            }
        ];

        tasksService.getAllVigilancePendingTasks.and.returnValue(of(vigilanceTasksMock));

        component.getVigilanceTasks();

        expect(tasksService.getAllVigilancePendingTasks).toHaveBeenCalled();
        expect(component.pendingTaskList.length).toBe(3);
        expect(component.pendingTaskList.map(task => task.description)).toEqual(['Vigilance 1', 'Vigilance 2', 'Vigilance 3']);
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

    it('should update pending task list', () => {
        const tasksMock: TaskViewModel[] = [
            { id: '1', description: 'Task 1', user: 'Pedro', contact: 12341412214, type: 'Pickup' },
            { id: '2', description: 'Task 2', user: 'Cristina', contact: 24112341414, type: 'Pickup' }
        ];
        component.pendingTaskList = [];

        component.updatePendingTaskList(tasksMock);

        expect(component.pendingTaskList.length).toBe(2);
        expect(component.pendingTaskList.map(task => task.description)).toEqual(['Task 1', 'Task 2']);
    });

});