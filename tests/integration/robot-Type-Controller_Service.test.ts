import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import IRobotTypeService from "../../src/services/IServices/IRobotTypeService";
import RobotTypeController from "../../src/controllers/robotTypeController";
import { RobotType } from '../../src/domain/robotType';

describe('Integration test robot type controller -> service ', function () {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		this.timeout(6000);
		Container.reset();
		let robotTypeSchemaInstance = require("../../src/persistence/schemas/robotTypeSchema").default;
		Container.set("robotTypeSchema", robotTypeSchemaInstance);

		let robotTypeRepoClass = require("../../src/repos/robotTypeRepo").default;
		let robotTypeRepoInstance = Container.get(robotTypeRepoClass);
		Container.set("RobotTypeRepo", robotTypeRepoInstance);

		let robotTypeInstance = require("../../src/domain/robotType").RobotType;
		Container.set("RobotType", robotTypeInstance);

		let robotTypeServiceClass = require("../../src/services/robotTypeService").default;
		let robotTypeServiceInstance = Container.get(robotTypeServiceClass);
		Container.set("RobotTypeService", robotTypeServiceInstance);

    });

	afterEach(function() {
		sinon.restore();
		sandbox.restore();
	});

    it('RobotTypeController -> robotTypeService integration test using robotTypeRepo and RobotType stub (createRobotType)', async function () {
		// Arrange
        let body = { "id":'123', "designacao": "TipoX", "tipoTarefas": ["Segurança"] };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let robotTypeRepoInstance = Container.get("RobotTypeRepo");
		sinon.stub(robotTypeRepoInstance, "save").returns(new Promise<RobotType>((resolve, reject) => {
			resolve(RobotType.create({
				"id": req.body.id,
				"designacao": req.body.designacao,
				"tipoTarefas": req.body.tipoTarefas
			}).getValue())
		}));

		let robotTypeInstance = Container.get("RobotType");
		const robotTypeStub = sinon.stub(robotTypeInstance, "create").returns( Result.ok( {
			"id":"123", 
			"designacao": req.body.designacao,
			"tipoTarefas": req.body.tipoTarefas} 
		));


		let robotTypeServiceInstance = Container.get('RobotTypeService');
		const ctrl = new RobotTypeController(robotTypeServiceInstance as IRobotTypeService);
		
		// Act
		await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ 
			"id":"123", 
			"designacao": req.body.designacao,
			"tipoTarefas": req.body.tipoTarefas
		}));
		sinon.assert.calledOnce(robotTypeStub);
	});

	/*

	it('RobotTypeController -> robotTypeService integration test using robotTypeRepo and RobotType stub (updateRobotType)', async function () {
		// Arrange
        let body = { "id":'123', "designacao": "TipoX", "tipoTarefas": ["Segurança"] };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let robotTypeRepoInstance = Container.get("RobotTypeRepo");
		const saveStub =  sinon.stub(robotTypeRepoInstance, "save").returns(new Promise<RobotType>((resolve, reject) => {
			resolve(RobotType.create({
				"id": "123",
				"designacao": req.body.designacao,
				"tipoTarefas": req.body.tipoTarefas
			}).getValue())
		}));

		const findByDesignationOrTaskTypeStub = sinon.stub(robotTypeRepoInstance, "findByDesignationOrTaskType").returns(new Promise<RobotType[]>((resolve, reject) => {
			resolve([RobotType.create({
				"id": "123",
				"designacao": req.body.designacao,
				"tipoTarefas": req.body.tipoTarefas
			}).getValue()])
		}));

		let robotTypeServiceInstance = Container.get('RobotTypeService');
		const ctrl = new RobotTypeController(robotTypeServiceInstance as IRobotTypeService);

		// Act
		await ctrl.updateRobotType(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(findByDesignationOrTaskTypeStub);
		sinon.assert.calledWith(findByDesignationOrTaskTypeStub, sinon.match("123"));
		sinon.assert.calledOnce(saveStub);
		sinon.assert.calledWith(saveStub,sinon.match({
			"id":"123", 
			"designacao": req.body.designacao,
			"tipoTarefas": req.body.tipoTarefas}));
	});
	*/
	

});


