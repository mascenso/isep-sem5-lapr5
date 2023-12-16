import * as sinon from 'sinon';

import { Response, Request, NextFunction } from 'express';

import { Container } from 'typedi';
import config from "../../config";

import { Result } from '../core/logic/Result';

import ITaskService from "../services/IServices/ITaskService";
import TaskController from "./taskController";
import ITaskDTO from '../dto/ITaskDTO';

describe('task controller', function () {
	beforeEach(function() {
    });

    it('createTask: returns json with id+name values', async function () {
        let body = { "name":'task12' };
        let req: Partial<Request> = {};
		req.body = body;

        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let taskServiceClass = require(config.services.task.path).default;
		let taskServiceInstance = Container.get(taskServiceClass)
		Container.set(config.services.task.name, taskServiceInstance);

		taskServiceInstance = Container.get(config.services.task.name);
		sinon.stub(taskServiceInstance, "createTask").returns( Result.ok<ITaskDTO>( {"id":"123", "name": req.body.name} ));

		const ctrl = new TaskController(taskServiceInstance as ITaskService);

		await ctrl.createTask(<Request>req, <Response>res, <NextFunction>next);

		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ "id": "123","name": req.body.name}));
	});
});
