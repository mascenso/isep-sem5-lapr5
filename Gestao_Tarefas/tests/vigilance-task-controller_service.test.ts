import 'reflect-metadata';
import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../src/core/logic/Result';
import TaskController from "../src/controllers/taskController";
import ITaskService from "../src/services/IServices/ITaskService";
import { TaskVigilance } from '../src/domain/task-agg/TaskVigilance';


describe('Integration test task vigilance controller -> service ', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        this.timeout(6000);
        Container.reset();

		let taskVigilanceSchemaSchemaInstance = require("../src/persistence/schemas/TaskVigilanceSchema").default;
		Container.set("taskVigilanceSchema", taskVigilanceSchemaSchemaInstance);

        let taskVigilanceRepoClass = require("../src/repos/TaskVigilanceRepo").default;
        let taskVigilanceRepoInstance = Container.get(taskVigilanceRepoClass);
        Container.set("taskVigilanceRepo", taskVigilanceRepoInstance);

        let taskVigilanceInstance = require("../src/domain/task-agg/TaskVigilance").TaskVigilance;
        Container.set("TaskVigilance", taskVigilanceInstance);
        
        let taskSchemaInstance = require("../src/persistence/schemas/TaskSchema").default;
		Container.set("taskSchema", taskSchemaInstance);

		let taskRepoClass = require("../src/repos/taskRepo").default;
		let taskRepoInstance = Container.get(taskRepoClass);
		Container.set("TaskRepo", taskRepoInstance);

   

        let taskServiceServiceClass = require("../src/services/taskService").default;
        let taskServiceServiceInstance = Container.get(taskServiceServiceClass);
        Container.set("TaskService", taskServiceServiceInstance);

    });

    afterEach(function () {
        // Restaurar os stubs e sinons
        sinon.restore();
        sandbox.restore();
    });
/*
    it('TaskVigilanceController -> TaskVigilanceService integration test (createVigilanceTask)', async function () {
        // Arrange
        const body = {
            "description": "Descrição da tarefa de vigilância",
            "buildingId": "ID Building GRANDE",
            "floors": ["Piso 1", "Piso 2"],
            "startPosition": [10, 20],
            "endPosition": [30, 40],
            "contactNumber": 123456789,
            "user": {
                "userName": "ze",
                "userContact": 1234578
            },
            "approved": false,
            "pending": true,
            "planned": false
        };
        const req: Partial<Request> = {
            body: body
        };
        const res: Partial<Response> = {
            json: sinon.spy()
        };
        const next: Partial<NextFunction> = () => { };


        let taskVigilanceRepoRepoInstance = Container.get("taskVigilanceRepo");
        sinon.stub(taskVigilanceRepoRepoInstance, "save").returns(new Promise<TaskVigilance>((resolve, reject) => {
            resolve(TaskVigilance.create({
                "description": req.body.description,
                "buildingId": req.body.buildingId,
                "floors": req.body.floors,
                "startPosition": req.body.startPosition,
                "endPosition": req.body.endPosition,
                "contactNumber": req.body.contactNumber,
                "user": req.body.user,
                "approved": req.body.approved,
                "pending": req.body.pending,
                "planned": req.body.planned
            }).getValue())
        }));

        let taskVigilanceInstance = Container.get("TaskVigilance");
        const vigilanceTaskStub = sinon.stub(taskVigilanceInstance, "create").returns(Result.ok({
            "id": "123",
            "description": req.body.description,
            "buildingId": req.body.buildingId,
            "floors": req.body.floors,
            "startPosition": req.body.startPosition,
            "endPosition": req.body.endPosition,
            "contactNumber": req.body.contactNumber,
            "user": req.body.user,
            "approved": req.body.approved,
            "pending": req.body.pending,
            "planned": req.body.planned
        }
        ));
        // Stub para simular o serviço do TaskVigilance
        const taskServiceServiceInstance = Container.get('TaskService');
        const ctrl = new TaskController (taskServiceServiceInstance as ITaskService);


        // Act
		await ctrl.createVigilanceTask(<Request>req, <Response>res, <NextFunction>next);

        sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({
			"id":"123",
            "description": req.body.description,
            "buildingId": req.body.buildingId,
            "floors": req.body.floors,
            "startPosition": req.body.startPosition,
            "endPosition": req.body.endPosition,
            "contactNumber": req.body.contactNumber,
            "user": req.body.user,
            "approved": req.body.approved,
            "pending": req.body.pending,
            "planned": req.body.planned
		}));
        sinon.assert.calledOnce(vigilanceTaskStub);
    });
    */
});
